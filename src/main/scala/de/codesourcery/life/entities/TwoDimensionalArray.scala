package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * A generic two-dimensional array that automatically wraps
 * invalid indices.
 *  
 * @param b
 */
class TwoDimensionalArray[T](val width:Int , val height:Int) {

	val data = new Array[Array[Any]](width)
	
	private def realX(x:Int) = {
		val result = x % width
		if ( result >= 0 ) {
			result
		} else {
			result + width
		}
	}
	
	private def realY(y:Int) = {
		val result = y % height
		if ( result >= 0 ) {
			result
		} else {
			result + height
		}
	}
		
	def printBoard() {
		
		var lastY : Int = -1
		visitAll( ( x , y , set ) => {
			if ( y != lastY && lastY > -1 ) {
				println
			}
			lastY = y
			print( set +" | ")
		} )
	}
	
	def set( x : Int , y : Int , value : T ) : TwoDimensionalArray[T] = {
		var array = data( realX( x ) )
		if ( array == null ) {
			array = new Array[Any]( height )
			data( realX( x ) ) = array
		}
		array( realY( y ) ) = value
		this
	}
	
	def get(x:Int,y:Int) : Option[T] = {
		val array = data( realX(x) )
		if ( array == null ) {
			None
		} else {
			Some( array(realY(y) ).asInstanceOf[T] )
		}
	}
	
	def visitAll( func : => (Int,Int,Option[T]) => Unit ) {
		visit( 0 , 0 , width-1 , height - 1 , func )
	}
	
	def visit(startX:Int,startY:Int,endX:Int,endY:Int, func : => (Int,Int,Option[T]) => Unit ) {
		for ( currentY <- startY to endY ; currentX <- startX to endX ) { 
			func( currentX , currentY , get( currentX , currentY ) )
		}
	}
	
}

object TwoDimensionalArray {
	
	def apply[T]( width : Int , height : Int ) : TwoDimensionalArray[T] = new TwoDimensionalArray[T]( width , height )
	
}
