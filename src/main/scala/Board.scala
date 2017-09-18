package xyz.hyperreal.chess

import math.{abs, signum}

import xyz.hyperreal.table.TextTable

import scala.collection.mutable.ArrayBuffer


abstract class Player( val sym: String )
case object White extends Player( "w" )
case object Black extends Player( "b" )

abstract class Type( val shortName: String, val longName: String )
case object Pawn extends Type( "p", "pawn" )
case object Knight extends Type( "n", "knight" )
case object Bishop extends Type( "b", "bishop" )
case object Rook extends Type( "r", "rook" )
case object Queen extends Type( "q", "queen" )
case object King extends Type( "k", "king" )

case class Piece( color: Player, typ: Type ) {
	def sym = color.sym + typ.shortName

	def isPawn = typ == Pawn

	def isKnight = typ == Knight

	def isBishop = typ == Bishop

	def isRook = typ == Rook

	def isQueen = typ == Queen

	def isKing = typ == King
}

object wp extends Piece( White, Pawn )
object wn extends Piece( White, Knight )
object wb extends Piece( White, Bishop )
object wr extends Piece( White, Rook )
object wq extends Piece( White, Queen )
object wk extends Piece( White, King )
object bp extends Piece( Black, Pawn )
object bn extends Piece( Black, Knight )
object bb extends Piece( Black, Bishop )
object br extends Piece( Black, Rook )
object bq extends Piece( Black, Queen )
object bk extends Piece( Black, King )


case class Square( x: Int, y: Int )

trait Action
case class Capture( s: Square, removed: Piece ) extends Action
case class Move( src: Square, dst: Square ) extends Action

class Board {
	private val pieces = Array.ofDim[Piece]( 8, 8 )
	private val actions = new ArrayBuffer[Action]

	init

	def square( x: Int, y: Int ) = pieces(x)(y)

	def square( s: Square ): Piece = square( s.x, s.y )

	def square( x: Int, y: Int, p: Piece ) = pieces(x)(y) = p

	def square( s: Square, p: Piece ) {square( s.x, s.y, p )}

	def occupied( s: Square ) = square( s ) ne null

	def empty( s: Square ) = square( s ) eq null

	def obstructed( src: Square, dst: Square ): Boolean = {
		val incx = signum( dst.x - src.x )
		val incy = signum( dst.y - src.y )

		var x = src.x
		var y = src.y

		def traverse: Boolean = {
			x += incx
			y += incy

			if (x != dst.x || y != dst.y)
				if (pieces(x)(y) eq null)
					traverse
				else
					true
			else
				false
		}

		traverse
	}

	def valid( src: Square, dst: Square ) = {
		val xd = abs( src.x - dst.x )
		val yd = abs( src.y - dst.y )

		square( src ) match {
			case Piece( c, Pawn ) => // todo: en passant
				if (occupied( dst ))
					xd == 1 && yd == 1 && (c == White && src.y < dst.y || c == Black && src.y > dst.y)
				else
					xd == 0 && yd == 1 && (c == White && src.y < dst.y || c == Black && src.y > dst.y) ||
						!obstructed( src, dst ) && xd == 0 && yd == 2 && (c == White && src.y < dst.y && src.y == 1 || c == Black && src.y > dst.y && src.y == 6)
			case Piece( _, Knight ) => xd == 1 && yd == 2 || xd == 2 && yd == 1
			case Piece( _, Bishop ) => xd == yd && !obstructed( src, dst )
			case Piece( _, Rook ) => (xd == 0 || yd == 0) && !obstructed( src, dst )
			case Piece( _, Queen ) => (xd == 0 || yd == 0 || xd == yd) && !obstructed( src, dst )
			case Piece( _, King ) => xd <= 1 && yd <= 1
			case _ => sys.error( "unknown type of piece" )
		}
	}

	def move( src: Square, dst: Square ): Unit = {
		if (occupied( dst ))
			actions += Capture( dst, square(dst) )

		actions += Move( src, dst )
		square( dst, square(src) )
		square( src, null )
	}

	def canUndo = actions nonEmpty

	def undo: Unit = {
		actions remove (actions.length - 1) match {
			case Capture( s, removed ) => square( s, removed )
			case Move( src, dst ) =>
				square( src, square(dst) )
				square( dst, null )

				if (!actions.last.isInstanceOf[Move])
					undo
		}
	}

//	def move( sx: Int, sy: Int, dx: Int, dy: Int ): Unit = {
//		require( full(sx, sy), "starting square is empty" )
//		require( empty(dx, dy), "destination square is full" )
//		pieces(dx)(dy) = pieces(sx)(sy)
//	}

//	def capture( sx: Int, sy: Int, dx: Int, dy: Int ) = {
//		require( full(sx, sy), "starting square is empty" )
//		require( full(dx, dy), "destination square is empty" )
//
//		val captured = pieces(dx)(dy)
//
//		pieces(dx)(dy) = pieces(sx)(sy)
//		captured
//	}

	def init: Unit = {
		def back( col: Player, y: Int ) =
			for ((c, x) <- List( Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ) zipWithIndex)
				pieces(x)(y) = Piece( col, c )

		def front( col: Player, y: Int ) =
			for (x <- 0 until 8)
				pieces(x)(y) = Piece( col, Pawn )

		for (x <- 0 until 8; y <- 2 until 6)
			pieces(x)(y) = null

		back( Black, 7 )
		front( Black, 6 )
		front( White, 1 )
		back( White, 0 )
	}

	def show: Unit = {
		val t =
			new TextTable( boxedTable = true, columnDividers = true ) {
				for (y <- 7 to 0 by -1) {
					rowSeq(
						/*(y + 1) +:*/ for (x <- 0 to 7) yield
							pieces( x )( y ) match {
								case null => ""
								case p => p.sym
							} )
					line
				}

//				rowSeq( "" +: ('a' to 'h') )
			}

		print( t )
	}
}
