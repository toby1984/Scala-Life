package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * A torus that holds boolean values and uses a bitfield as it's underlying
 * storage.
 * 
 * @author Tobias.Gierke@code-sourcery.de
 */
class BitfieldTorus( private val data : TwoDimensionalStorage[Boolean] ) extends Torus[Boolean](data) {

	def this(width:Int,height:Int) {
		this( new BitfieldStorage(width,height) )
	}
	
	def createCopy() : BitfieldTorus = {
		new BitfieldTorus( data.createCopy() )
	}
}

private class BitfieldStorage(val w:Int,val h:Int) extends TwoDimensionalStorage[Boolean](w,h) {
	
	// number of bits per array element
	private val ARRAY_ELEMENT_BIT_WIDTH = 32
	
	// I use shift operations instead of divide / multiply here
	private val SHIFT_BITCOUNT = 6 // = 2^6 = 32 
	
	private val paddedWidth : Int =  if ( ( w % ARRAY_ELEMENT_BIT_WIDTH ) != 0 ) {
		val tmp :Int = Math.ceil( w.asInstanceOf[Float] / ARRAY_ELEMENT_BIT_WIDTH.asInstanceOf[Float] ).asInstanceOf[Int]
		tmp * ARRAY_ELEMENT_BIT_WIDTH
	} else {
		w 
	}
	
	private var data : Array[Int]= new Array[Int]( calcLength(w,h ) )
	
	def calcLength(aWidth : Int, aHeight : Int ) : Int = {
		Math.ceil( ( aHeight * paddedWidth ) / ARRAY_ELEMENT_BIT_WIDTH ).asInstanceOf[Int]
	}
	
	def clear() {
		data = new Array[Int]( calcLength(w,h ) )
	}
	
	def getValueAt(x:Int,y:Int) : Boolean = {
		
		val bitOffset : Int = x + y* paddedWidth
		val byteOffset : Int = bitOffset / ARRAY_ELEMENT_BIT_WIDTH 
		val offsetInByte : Int = bitOffset -( byteOffset * ARRAY_ELEMENT_BIT_WIDTH )
		
		return ( data(byteOffset) & 1 << offsetInByte ) != 0
	}
	
	def setValueAt(x:Int,y:Int,value:Boolean) {
		
		val bitOffset : Int = x + y* paddedWidth
		val byteOffset : Int = bitOffset / ARRAY_ELEMENT_BIT_WIDTH 
		val offsetInByte : Int = bitOffset -( byteOffset * ARRAY_ELEMENT_BIT_WIDTH )
		
		if ( value ) { // set bit
			data(byteOffset) = ( data(byteOffset) | 1 << offsetInByte)
		} else { // clear bit
			// val mask = ALL_ONES_MASK - ( 1 << offsetInByte )
			data(byteOffset) = (data(byteOffset) & ~(1 << offsetInByte) )
		}
	}
	 
	def createCopy() : TwoDimensionalStorage[Boolean] = {
	 		val result = new BitfieldStorage( w , h )
	 		Array.copy( data , 0 , result.data , 0 , data.length )
	 		result
	}
	
	def createCopy(newWidth:Int,newHeight:Int) : TwoDimensionalStorage[Boolean] = {

 		val result = new BitfieldStorage( newWidth , newHeight )
 		val maxLength = if ( result.data.length <= data.length ) result.data.length else data.length  
 		Array.copy( data , 0 , result.data , 0 , maxLength )
 		result
	}	
}