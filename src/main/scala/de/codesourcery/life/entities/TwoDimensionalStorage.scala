package de.codesourcery.life.entities

/**
 * A generic two-dimensional storage.
 *  
 * @author tobias.gierke@code-sourcery.de
 */
abstract class TwoDimensionalStorage[T](val width:Int,val height:Int) {

	/**
	 * Returns the value at a given X/Y coordinate.
	 * 
	 * @param x X coordinate (range 0...height-1)
	 * @param y Y coordinate (range 0...width-1)
	 * @return the value
	 * @thows ArrayIndexOutOfBoundsException if the coordinates
	 * are beyond the actual extend (less than zero
	 * or equals or greater than width / height )
	 */
	def getValueAt(x:Int,y:Int) : T
	
	/**
	 * Stores a value at a given X/Y coordinate.
	 * 
	 * @param x X coordinate (range 0...height-1)
	 * @param y Y coordinate (range 0...width-1)
	 * @param value the value to store
	 * @thows ArrayIndexOutOfBoundsException if the coordinates
	 * are beyond the actual extend (less than zero
	 * or equals or greater than width / height )
	 */	
	def setValueAt(x:Int,y:Int,value:T) : Unit
	
	/**
	 * Creates an independent copy of this
	 * instance.
	 * 
	 * @return
	 */
	def createCopy() : TwoDimensionalStorage[T]
	
	/**
	 * Creates an independent copy of this
	 * instance with a specific size.
	 * 
	 * @return
	 */
	def createCopy(width:Int,height:Int) : TwoDimensionalStorage[T]	
	
    /**
	 * Resets this storage's content
	 * to it's initial state.
	 */
	def clear() :Unit
	
}