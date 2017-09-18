package xyz.hyperreal.chess

import java.awt.{Color, Point}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.swing._
import Swing._
import scala.swing.event._


class BoardPanel( board: Board ) extends Panel {
	val square = 75
	val white = new Color( 240, 217, 181 )//Color.BLUE.brighter.brighter.brighter.brighter
	val black = new Color( 181, 136, 99 )//Color.BLUE.darker.darker.darker.darker
	val images =
		Map(
			(for (f <- List( "wp", "wn", "wb", "wr", "wq", "wk", "bp", "bn", "bb", "br", "bq", "bk" ))
				yield f -> ImageIO.read( new File(s"images/$f.svg.png"))): _* )
	var selection: Square = _
	var point: Point = _
	var image: BufferedImage = _
	var orientation = true

	preferredSize = (square*8, square*8)
	listenTo( mouse.clicks, mouse.moves/*, keys*/ )

	reactions += {
		case e: MousePressed =>
			requestFocusInWindow

			if (selection eq null) {
				point = new Point( e.point.x, e.point.y )

				val loc = point2square( point )

				board.square( loc ) match {
					case null =>
					case p =>
						selection = loc
						image = images(p.sym)
						repaint
				}
			}

		case e: MouseDragged =>
			point = new Point( e.point.x, e.point.y )
			repaint
		case e: MouseReleased =>
			if (selection ne null) {
				val dest = point2square( e.point )

				if (dest.x != selection.x || dest.y != selection.y) {
					if (board.valid( selection, dest ))
						board.move( selection, dest )
					else
						Dialog.showMessage( this, s"a ${board.square(selection).typ.name} can't move like that", "Error", Dialog.Message.Error )
				}

				selection = null
				repaint
			}
//		case e: MouseMoved => println( e.point.x, e.point.y )
//		case e: MouseEntered =>
		case _: MouseExited =>
			selection = null
			repaint
	}

	def point2square( p: Point ) = {
		if (orientation)
			Square( p.x/square, 7 - p.y/square )
		else
			Square( 7 - p.x/square, p.y/square )
	}

	def switch: Unit = {
		orientation = !orientation
		repaint
	}

	def color( x: Int, y: Int ): Player =
		if (((x&1) ^ (y&1)) == 0)
			Black
		else
			White

	override def paintComponent( g: Graphics2D ): Unit = {
		super.paintComponent( g )

		for (i <- 0 until 8; j <- 0 until 8) {
			val x =
				if (orientation)
					i
				else
					7 - i
			val y =
				if (orientation)
					j
				else
					7 - j

			g.setColor( if (color(x, y) == White) white else black )
			g.fillRect( i*square, (7 - j)*square, square, square )

			board.square( x, y ) match {
				case null =>
				case p if selection == null || selection.x != x || selection.y != y => g.drawImage( images(p.sym), i*square, (7 - j)*square, null )
				case _ =>
			}
		}

		selection match {
			case Square( _, _ ) =>
				g.drawImage( image, point.x - square/2, point.y - square/2, null )
			case null =>
		}
	}
}
