package de.codesourcery.life.entities

import scala.collection.mutable.HashSet

class HashSetTorus(private var data : TwoDimensionalStorage[Boolean]) extends Torus[Boolean]( data ) {

		def this(w:Int,h:Int) {
			this( new HashStorage(w,h) )
		}
		
		def createCopy() : HashSetTorus = {
			new HashSetTorus( getData().createCopy )
		}
		
		def visitAlive( func : => (Int,Int) => Unit ) = {
			
			val wrapperFunction = (x:Int,y:Int,isSet:Boolean) => {
				if ( isSet ) {
					func( x , y )
				}
			}
			visitAll( wrapperFunction )
		}
}

class HashStorage(w:Int,h:Int) extends TwoDimensionalStorage[Boolean](w,h) {
	
	private val set = HashSet[Integer]()
	
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
	def getValueAt(x:Int,y:Int) : Boolean = {
		set.contains( ( 23+ (x * 31) << 16 | y ) ) 
	}
	
	/**
	 * Stores a value at a given X/Y coordinate.
	 * 
	 * @param x X coordinate (range 0...height-1)
	 * @param y Y coordinate (range 0...width-1)
	 * @param value the value to store
	 * @return the old value
	 * @thows ArrayIndexOutOfBoundsException if the coordinates
	 * are beyond the actual extend (less than zero
	 * or equals or greater than width / height )
	 */	
	def setValueAt(x:Int,y:Int,newValue:Boolean) : Boolean = {
		val hash = ( 23+ (x * 31) << 16 | y )
		val oldValue = set.contains( hash )
		if  ( oldValue != newValue && newValue == true) {
			set.add( hash )
		}
		oldValue
	}
	
	/**
	 * Creates an independent copy of this
	 * instance.
	 * 
	 * @return
	 */
	def createCopy() : TwoDimensionalStorage[Boolean] = {
		val result = new HashStorage(w,h)
		result.set ++= set
		result
	}
	
	/**
	 * Creates an independent copy of this
	 * instance with a specific size.
	 *
	 * <p>If the target is smaller than this
	 * storage, this method will ignore
	 * all elements that do not fall within the 
	 * bounds of the target.</p
	 * @return
	 */
	def createCopy(width:Int,height:Int) : TwoDimensionalStorage[Boolean] = {
		// TODO: Could discard all entries that do not fit into the new one
		val result = new HashStorage(w,h)
		result.set ++= set
		result		
	}
	
    /**
	 * Resets this storage's content
	 * to it's initial state.
	 */
	def clear()  {
		set.clear()
	}
}