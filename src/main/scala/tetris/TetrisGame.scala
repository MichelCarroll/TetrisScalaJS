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

    case class GameContext(staticBlocks: Set[StaticBlock], mobileShape: Option[MobileShape])
    case class CanvasPosition(x: Double, y: Double)

    case class BlockPosition(x: Int, y: Int) {
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

    case class StaticBlock(position: BlockPosition, color: BlockColor)
    case class MobileShape(definition: MobileShapeDefinition, position: BlockPosition, rotationPosition: RotationPosition) {

      def dropped = MobileShape(definition, position.down, rotationPosition)
      def blocks = definition.blocks
          .map(_.applyRotation(rotationPosition.matrix, definition.center))
          .map(_.add(position))

      def staticBlocks = blocks.map(StaticBlock(_, definition.color)).toSet
      def isInvalid(implicit staticBlocks: Set[StaticBlock]) =
        blocks.exists(block => block.isOutOfBounds || block.isOverlapping(staticBlocks.map(_.position)))

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

    def allValidMobileShapes(implicit gameContext: GameContext): Set[MobileShapeDefinition] = {

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
        .filterNot(_.blocks.exists(_.isOverlapping(gameContext.staticBlocks.map(_.position))))
    }

    implicit var gameContext = GameContext(Set[StaticBlock](), None)

    def randomShapeDefinition: Option[MobileShapeDefinition] = {
      val permutations = allValidMobileShapes
      if(permutations.isEmpty)
        None
      else
        Some(permutations.toList(nextInt(permutations.size)))
    }

    gameContext = GameContext(
      gameContext.staticBlocks,
      randomShapeDefinition.flatMap(shape =>
        Some(MobileShape(shape, BlockPosition(0,0), IdentityRotationPosition))
      )
    )

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, gridWidth * gridEdgeSize,  gridHeight * gridEdgeSize)
    }

    def drawBlock(blockPosition: BlockPosition, blockColor: BlockColor): Unit = {

      val position = CanvasPosition(blockPosition.x * gridEdgeSize, blockPosition.y * gridEdgeSize)
      ctx.fillStyle = blockColor.name
      ctx.fillRect(position.x, position.y, gridEdgeSize, gridEdgeSize)
      ctx.strokeStyle = "white"
      ctx.strokeRect(position.x, position.y, gridEdgeSize, gridEdgeSize)
    }

    def draw()(implicit gameContext: GameContext): Unit = {
      clear()

      gameContext.mobileShape match {
        case Some(mobileShape) =>
          (mobileShape.staticBlocks ++ gameContext.staticBlocks).foreach { staticBlock =>
            drawBlock(blockPosition = staticBlock.position, blockColor = staticBlock.color)
          }
        case None =>
          gameContext.staticBlocks.foreach {  staticBlock =>
            drawBlock(blockPosition = staticBlock.position, blockColor = staticBlock.color)
          }
      }
    }

    def update()(implicit gameContext: GameContext): GameContext = {
      gameContext.mobileShape match {
        case Some(mobileShape) => {
          val newMobileShape = mobileShape.dropped

          val invalidNewMobileBlockPosition =
            newMobileShape.blocks.exists { position =>
              position.isOutOfBounds || position.isOverlapping(gameContext.staticBlocks.map(_.position))
            }

          if(invalidNewMobileBlockPosition) {
            GameContext(
              gameContext.staticBlocks ++ mobileShape.staticBlocks,
              randomShapeDefinition.flatMap(shape =>
                Some(MobileShape(shape, BlockPosition(0,0), IdentityRotationPosition))
              )
            )
          } else {
            GameContext(
              gameContext.staticBlocks,
              Some(newMobileShape)
            )
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
            GameContext(gameContext.staticBlocks, Some(newShape))
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
    dom.setInterval(() => loop(), 500)
  }
}