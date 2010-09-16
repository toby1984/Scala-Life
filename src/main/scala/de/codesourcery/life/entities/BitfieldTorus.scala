package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * A bitfield.
 *  
 * @param b
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
	
	private var data = {
		val len = ( ( w*h ) / 8 )+ 1
		new Array[Byte]( len )
	}
	
	def getValueAt(x:Int,y:Int) : Boolean = {
		val bitOffset : Int = x + y*h
		val byteOffset : Int = ( bitOffset / 8 )
		val offsetInByte : Int = bitOffset -( byteOffset * 8 )
		return ( data(byteOffset) & 1 << offsetInByte ) != 0
	}
	
	def setValueAt(x:Int,y:Int,value:Boolean) {
		val bitOffset : Int = x+y*h
		val byteOffset : Int = ( bitOffset / 8 )
		val offsetInByte : Int = bitOffset-( byteOffset * 8 )
		if ( value ) {
			data(byteOffset) = ( data(byteOffset) | 1 << offsetInByte).asInstanceOf[Byte]
		} else {
			val mask = 255 - ( 1 << offsetInByte )
			data(byteOffset) = (data(byteOffset) & mask).asInstanceOf[Byte]
		}
	}
	 
	def createCopy() : TwoDimensionalStorage[Boolean] = {
	 		val result = new BitfieldStorage( w , h )
	 		result.data = new Array[Byte]( data.length )
	 		Array.copy( data , 0 , result.data , 0 , data.length )
	 		result
	}
}