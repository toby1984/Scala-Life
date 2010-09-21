package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * Implements a two-dimensional array where read/write access beyond
 * the actual extent wraps around.
 * 
 *  <p>
 *  This is the 'playing field' data gets stored
 *  in a convenient way.
 *  </p>
 * @param b
 */
abstract class Torus[T]( private var data : TwoDimensionalStorage[T]) {

	/**
	 * Returns the actual width of the array.
	 * 
	 * Accessing elements with an X-coordinate
	 * beyond the actual width will automatically
	 * wrap around.
	 * @return
	 */
	def width = data.width

	/**
	 * Returns the actual height of the array.
	 * 
	 * Accessing elements with an Y-coordinate
	 * beyond the actual height will automatically
	 * wrap around.
	 * 
	 * @return
	 */	
	def height = data.height
	
	/**
	 * Creates an independent copy of this
	 * torus.
	 *  
	 * @return
	 */
	def createCopy() : Torus[T]
	
	/**
	 * Copies data from this instance
	 * onto another.
	 * 
	 * The target becomes an independent
	 * copy of this instance.
	 * 
	 * @param target the instance to populate
	 * @return THIS instance
	 */
	def populateCopy( target : Torus[T] ) : Torus[T] = {
		target.data = data.createCopy()
		this
	}
	
	/**
	 * Sets the data this torus represents.
	 * 
	 * @param value
	 */
	def setData( value : TwoDimensionalStorage[T] ) {
		this.data = value
	}
	
	/**
	 * Returns the data this torus represents.
	 * 
	 * @return the storage that holds this instance's actual data.
	 */
	def getData() : TwoDimensionalStorage[T] = data
	
	/**
	 * Maps an X coordinate
	 * to the actual X array index.
	 * 
	 * <p>
	 * This method makes sure the X coordinate
	 * wraps around when it exceeds
	 * the actual width of the underlying
	 * storage.
	 * </p>
	 * 
	 * @param x an X coordinate
	 * @return a valid array index into the X (width) array
	 */
	private def realX(x:Int) = {
		val result = x % (width-1)
		if ( result >= 0 ) {
			result
		} else {
			result + width
		}
	}
	
	/**
	 * Maps an Y coordinate
	 * to the actual Y array index.
	 * 
	 * <p>
	 * This method makes sure the Y coordinate
	 * wraps around when it exceeds
	 * the actual height of the underlying
	 * storage.
	 * </p>
	 * 
	 * @param y an Y coordinate
	 * @return a valid array index into the Y (height) array
	 */	
	private def realY(y:Int) = {
		val result = y % (height-1)
		if ( result >= 0 ) {
			result
		} else {
			result + height
		}
	}
	
	/**
	 * Stores a given value at a specific X-Y coordinate.
	 * 
	 * <p>
	 * Coordinates below zero or above the actual
	 * extend ( width , height ) will be wrapped 
	 * around.
	 * </p>
	 * @param x
	 * @param y
	 * @param value the value to store
	 * @return <code>this</code> instance (for chaining)
	 */
	def set( x : Int , y : Int , value : T ) : Torus[T] = {
		data.setValueAt( realX( x ) , realY( y ), value  )
		this
	}
	
	def resize(newWidth:Int,newHeight:Int) {
		require( newWidth > 0 )
		require( newHeight > 0 )
		setData( getData().createCopy( newWidth , newHeight ) )
	}
		
	/**
	 * Returns the value at given X-Y coordinate.
	 * 
	 * <p>
	 * Coordinates below zero or above the actual
	 * extend ( width , height ) will be wrapped 
	 * around.
	 * </p>	 
	 * @param x
	 * @param y
	 * @return
	 */
	def get(x:Int,y:Int) : T = data.getValueAt( realX(x) , realY(y) )
	
	/**
	 * Accepts a function that gets called once for each
	 * X-Y coordinate (actual extent).
	 * 
	 * @param func
	 */
	def visitAll( func : => (Int,Int,T) => Unit ) {
		visit( 0 , 0 , width-1 , height - 1 , func )
	}
	
	/**
	 * Accepts a function that gets called once for each
	 * X-Y coordinate (actual extent) within a 
	 * given sub-surface (box).
	 * 	 
	 * <p>
	 * Coordinates below zero or above the actual
	 * extend ( width , height ) will be wrapped 
	 * around.
	 * </p>	 	 
	 * @param startX
	 * @param startY
	 * @param endX
	 * @param endY
	 * @param func
	 */
	def visit(startX:Int,startY:Int,endX:Int,endY:Int, func : => (Int,Int,T) => Unit ) {

		var currentY = startY
		while ( currentY <= endY ) {
			
			val convertedY = realY( currentY )
			
			var currentX = startX
			while( currentX <= endX ) {
				val convertedX =
					realX( currentX ) 
				func( currentX , currentY , data.getValueAt( convertedX, convertedY ) )
				currentX += 1
			}
			currentY +=1
		}
	}
	
}
