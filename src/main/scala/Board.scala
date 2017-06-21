package xyz.hyperreal.chess

import xyz.hyperreal.table.TextTable

import scala.collection.mutable.ArrayBuffer


abstract class Player( val sym: String )
case object White extends Player( "w" )
case object Black extends Player( "b" )

abstract class Class( val sym: String )
case object Pawn extends Class( "p" )
case object Knight extends Class( "n" )
case object Bishop extends Class( "b" )
case object Rook extends Class( "r" )
case object Queen extends Class( "q" )
case object King extends Class( "k" )

case class Piece( color: Player, clazz: Class ) {
	def sym = color.sym + clazz.sym

	def isPawn = clazz == Pawn

	def isKnight = clazz == Knight

	def isBishop = clazz == Bishop

	def isRook = clazz == Rook

	def isQueen = clazz == Queen

	def isKing = clazz == King
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

case class Move( src: Square, dst: Square, piece: Piece )

object Board {
	def color( x: Int, y: Int ): Player =
		if (((x&1) ^ (y&1)) == 0)
			White
		else
			Black
}

class Board {
	private val pieces = Array.ofDim[Piece]( 8, 8 )
	private val moves = new ArrayBuffer[Move]

	init

	def square( x: Int, y: Int ) = pieces(x)(y)

	def square( s: Square ): Piece = square( s.x, s.y )

	def square( x: Int, y: Int, p: Piece ) = pieces(x)(y) = p

	def square( s: Square, p: Piece ) {square( s.x, s.y, p )}

	def move( src: Square, dst: Square ): Unit = {
		moves += Move( src, dst, square(dst) )
		square( dst, square(src) )
		square( src, null )
	}

	def canUndo = moves nonEmpty

	def undo: Unit = {
		val Move( src, dst, piece ) = moves remove (moves.length - 1)
		square( src, square(dst) )
		square( dst, null )
		square( dst, piece )
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

	def full( x: Int, y: Int ) = pieces(x)(y) ne null

	def empty( x: Int, y: Int ) = pieces(x)(y) eq null

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
			new TextTable {
				for (y <- 7 to 0 by -1) {
					rowSeq(
						/*(y + 1) +:*/ (for (x <- 0 to 7) yield
							pieces( x )( y ) match {
								case null => ""
								case p => p.sym
							}) )
					line
				}

//				rowSeq( "" +: ('a' to 'h') )
			}

		print( t )
	}
}
