package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * A generic two-dimensional array that automatically wraps
 * invalid indices.
 *  
 * @param b
 */
abstract class Torus[T]( private var data : TwoDimensionalStorage[T]) {

	def width = data.width
	def height = data.height
	
	def createCopy() : Torus[T]
	
	def populateCopy( other : Torus[T] ) : Torus[T] = {
		other.data = data.createCopy()
		this
	}
	
	def setData( value : TwoDimensionalStorage[T] ) {
		this.data = value
	}
	
	def getData() : TwoDimensionalStorage[T] = data
	
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
		
	def set( x : Int , y : Int , value : T ) : Torus[T] = {
		data.setValueAt( realX( x ) , realY( y ), value  )
		this
	}
	
	def get(x:Int,y:Int) : T = data.getValueAt( realX(x) , realY(y) )
	
	def visitAll( func : => (Int,Int,T) => Unit ) {
		visit( 0 , 0 , width-1 , height - 1 , func )
	}
	
	def visit(startX:Int,startY:Int,endX:Int,endY:Int, func : => (Int,Int,T) => Unit ) {

		var currentY = startY
		while ( currentY <= endY ) {
			var currentX = startX
			while( currentX <= endX ) {
				func( currentX , currentY , get( currentX , currentY ) )
				currentX += 1
			}
			currentY +=1
		}
	}
	
}