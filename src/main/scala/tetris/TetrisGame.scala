package tetris

import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport
import scala.util.Random.nextInt
import java.lang.Math.floor

import scala.annotation.tailrec
import scala.collection.immutable.Map

@JSExport
object TetrisGame {

  @JSExport
  def main(canvas: html.Canvas): Unit = {

    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    val canvasWidth = ctx.canvas.width
    val canvasHeight = ctx.canvas.height
    val gridEdgeSize = 20
    val gridWidth = floor(canvasWidth / gridEdgeSize).toInt
    val gridHeight = floor(canvasHeight / gridEdgeSize).toInt


    case class CanvasPosition(x: Double, y: Double)

    case class BlockPosition(x: Int, y: Int ) {
      def down = BlockPosition(x, y + 1)
      def up = BlockPosition(x, y - 1)
      def right = BlockPosition(x + 1, y)
      def left = BlockPosition(x - 1, y)
      def isOutOfBoundsFromTop = y < 0
      def isOutOfBoundsFromLeft = x < 0
      def isOutOfBounds = isOutOfBoundsFromLeft || isOutOfBoundsFromTop || x >= gridWidth || y >= gridHeight
      def isOverlapping(blocks: Set[BlockPosition]) = blocks.contains(this)
      def relativeTo(pos: BlockPosition) = BlockPosition(x - pos.x, y - pos.y)
      def applyRotation(matrix: List[List[Int]], center: BlockPosition): BlockPosition = {
        val relative = this.relativeTo(center)
        val newX = relative.x * matrix(0)(0) + relative.y * matrix(0)(1)
        val newY = relative.x * matrix(1)(0) + relative.y * matrix(1)(1)
        BlockPosition(
          newX + center.x,
          newY + center.y
        )
      }
      def add(pos: BlockPosition): BlockPosition = BlockPosition(x + pos.x, y + pos.y)
    }

    case class StaticBlock(position: BlockPosition, color: BlockColor)
    case class StaticBlockArea(blocks: Set[StaticBlock]) {
      def completeRows: Set[Int] = blocks
        .groupBy(_.position.y)
        .filter(_._2.size == gridWidth)
        .keySet

      def withClearedCompleteRows = {
        val rows = completeRows
        StaticBlockArea(
          blocks
            .filterNot(block => rows.contains(block.position.y))
            .map(block => StaticBlock(
              BlockPosition(
                block.position.x,
                block.position.y + rows.count(_ > block.position.y)
              ),
              block.color
            ))
        )
      }
    }

    implicit def areaToStaticBlocks(staticBlockArea: StaticBlockArea): Set[StaticBlock] =
      staticBlockArea.blocks

    implicit def staticBlocksToPositions(staticBlocks: Set[StaticBlock]): Set[BlockPosition] =
      staticBlocks.map(_.position)

    implicit def staticBlockAreaToPositions(staticBlockArea: StaticBlockArea): Set[BlockPosition] =
      staticBlockArea.blocks.map(_.position)

    case class BlockColor(name: String)
    case class MobileShapeDefinition(blocks: List[BlockPosition], center: BlockPosition, color: BlockColor)

    object IdentityRotationPosition extends RotationPosition(List(List(1, 0), List(0, 1)))
    object BaseRotationPosition extends RotationPosition(List(List(0, -1), List(1, 0)))

    case class RotationPosition(matrix: List[List[Int]]) {
      def next: RotationPosition = RotationPosition(List(
        List(
          matrix(0)(0) * BaseRotationPosition.matrix(0)(0) + matrix(1)(0) * BaseRotationPosition.matrix(0)(1),
          matrix(0)(1) * BaseRotationPosition.matrix(0)(0) + matrix(1)(1) * BaseRotationPosition.matrix(0)(1)
        ),
        List(
          matrix(0)(0) * BaseRotationPosition.matrix(1)(0) + matrix(1)(0) * BaseRotationPosition.matrix(1)(1),
          matrix(0)(1) * BaseRotationPosition.matrix(1)(0) + matrix(1)(1) * BaseRotationPosition.matrix(1)(1)
        )
      ))
    }

    case class MobileShape(definition: MobileShapeDefinition, position: BlockPosition, rotationPosition: RotationPosition) {

      def dropped = MobileShape(definition, position.down, rotationPosition)
      def blocks = definition.blocks
          .map(_.applyRotation(rotationPosition.matrix, definition.center))
          .map(_.add(position))

      def staticBlocks = blocks.map(StaticBlock(_, definition.color)).toSet
      def isInvalid(implicit gameContext: GameContext) =
        blocks.exists(block => block.isOutOfBounds || block.isOverlapping(gameContext.staticBlocks))

    }


    val shapeTemplates = Set(

      // **
      //  **
      MobileShapeDefinition(
        blocks = List(BlockPosition(0,1), BlockPosition(1,1), BlockPosition(1,0), BlockPosition(2,0)),
        center = BlockPosition(1,1),
        color = BlockColor("pink")
      ),
      //  **
      // **
      MobileShapeDefinition(
        blocks = List(BlockPosition(0,0), BlockPosition(1,0), BlockPosition(1,1), BlockPosition(2,1)),
        center = BlockPosition(1,0),
        color = BlockColor("red")
      ),

      // ***
      // *
      MobileShapeDefinition(
        blocks = List(BlockPosition(0,0), BlockPosition(1,0), BlockPosition(1,1), BlockPosition(1,2)),
        center = BlockPosition(1,1),
        color = BlockColor("blue")
      ),
      // *
      // ***
      MobileShapeDefinition(
        blocks = List(BlockPosition(1,0), BlockPosition(0,0), BlockPosition(0,1), BlockPosition(0,2)),
        center = BlockPosition(1,1),
        color = BlockColor("green")
      )
    )


    def allValidMobileShapes(gameContext: GameContext): Set[MobileShapeDefinition] = {

      val rotationMatrices = Set(
        BaseRotationPosition,
        BaseRotationPosition.next,
        BaseRotationPosition.next.next,
        BaseRotationPosition.next.next.next
      )

      def shapeRotationPermutations(shapeDefinition: MobileShapeDefinition): Set[MobileShapeDefinition] = {
        rotationMatrices
          .map(rotationMatrix =>
            shapeDefinition.blocks.map(_.applyRotation(rotationMatrix.matrix, shapeDefinition.center))
          )
          .map(rotatedShape => {
            @tailrec
            def correctedShapePositions(shape: List[BlockPosition]): List[BlockPosition] = {
              if(shape.exists(_.isOutOfBoundsFromTop))
                correctedShapePositions(shape.map(_.down))
              else if(shape.exists(_.isOutOfBoundsFromLeft))
                correctedShapePositions(shape.map(_.right))
              else
                shape
            }
            correctedShapePositions(rotatedShape)
          })
          .map(MobileShapeDefinition(_, shapeDefinition.center, shapeDefinition.color))
      }

      def shapePositionPermutations(shape: MobileShapeDefinition): Set[MobileShapeDefinition] = {
        if (shape.blocks.exists(_.isOutOfBounds))
          Set()
        else
          Set(shape) ++ shapePositionPermutations(
            MobileShapeDefinition(
              shape.blocks.map(_.right),
              shape.center.right,
              shape.color
            ))
      }

      shapeTemplates
        .flatMap(shapeRotationPermutations)
        .flatMap(shapePositionPermutations)
        .filterNot(_.blocks.exists(_.isOverlapping(gameContext.staticBlocks)))
    }

    case class GameContext(staticBlocks: StaticBlockArea, mobileShape: Option[MobileShape]) {

      def mergeBlocks(newBlocks: Set[StaticBlock]) = GameContext(
        StaticBlockArea(staticBlocks.blocks ++ newBlocks),
        mobileShape
      )

      private def randomShape: Option[MobileShapeDefinition] = {
        val permutations = allValidMobileShapes(this)
        if(permutations.isEmpty)
          None
        else
          Some(permutations.toList(nextInt(permutations.size)))
      }

      def replaceMobileShape(newShape: MobileShape) = GameContext(
        staticBlocks,
        Some(newShape)
      )

      def withRandomShape =
        GameContext(staticBlocks, this.randomShape.flatMap(shape =>
          Some(MobileShape(shape, BlockPosition(0,0), IdentityRotationPosition))
        ))

      def withClearedCompleteRows = GameContext(
        staticBlocks.withClearedCompleteRows,
        mobileShape
      )

    }


    implicit var gameContext = GameContext(StaticBlockArea(Set[StaticBlock]()), None)

    gameContext = GameContext(
      gameContext.staticBlocks,
      None
    )
    gameContext = gameContext.withRandomShape

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, gridWidth * gridEdgeSize,  gridHeight * gridEdgeSize)
    }

    def drawBlock(block: StaticBlock): Unit = {
      val position = CanvasPosition(block.position.x * gridEdgeSize, block.position.y * gridEdgeSize)
      ctx.fillStyle = block.color.name
      ctx.fillRect(position.x, position.y, gridEdgeSize, gridEdgeSize)
      ctx.strokeStyle = "white"
      ctx.strokeRect(position.x, position.y, gridEdgeSize, gridEdgeSize)
    }

    def draw()(implicit gameContext: GameContext): Unit = {
      clear()

      gameContext.mobileShape match {
        case Some(mobileShape) =>
          (mobileShape.staticBlocks ++ gameContext.staticBlocks).foreach(drawBlock)
        case None =>
          gameContext.staticBlocks.blocks.foreach(drawBlock)
      }
    }

    def update()(implicit gameContext: GameContext): GameContext = {
      gameContext.mobileShape match {
        case Some(mobileShape) => {
          val newMobileShape = mobileShape.dropped

          val invalidNewMobileBlockPosition =
            newMobileShape.blocks.exists { position =>
              position.isOutOfBounds || position.isOverlapping(gameContext.staticBlocks)
            }

          if(invalidNewMobileBlockPosition) {
            gameContext
              .mergeBlocks(mobileShape.staticBlocks)
              .withRandomShape
              .withClearedCompleteRows
          } else {
            gameContext
              .replaceMobileShape(newMobileShape)

          }
        }
        case None => gameContext
      }
    }

    def loop() = {
      draw()
      gameContext = update()
    }

    def executeCommand(keyCode: Int)(implicit gameContext: GameContext): GameContext = {
      gameContext.mobileShape match {
        case Some(mobileShape) => {

          val newShape = keyCode match {
            case 37 => //left
              MobileShape(mobileShape.definition, mobileShape.position.left, mobileShape.rotationPosition)
            case 38 => //up
              MobileShape(mobileShape.definition, mobileShape.position.up, mobileShape.rotationPosition)
            case 39 => //right
              MobileShape(mobileShape.definition, mobileShape.position.right, mobileShape.rotationPosition)
            case 40 => //down
              MobileShape(mobileShape.definition, mobileShape.position.down, mobileShape.rotationPosition)
            case 32 => //shape
              MobileShape(mobileShape.definition, mobileShape.position, mobileShape.rotationPosition.next)
            case _ => mobileShape
          }

          implicit val gameStaticBlocks = gameContext.staticBlocks

          if(!newShape.isInvalid) {
            gameContext.replaceMobileShape(newShape)
          } else {
            gameContext
          }

        }
        case None => gameContext
      }
    }

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      gameContext = executeCommand(e.keyCode)
      draw()
    }
    loop()
    dom.setInterval(() => loop(), 200)
  }
}