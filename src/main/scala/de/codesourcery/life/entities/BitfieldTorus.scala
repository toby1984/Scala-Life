package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * A torus that holds boolean values and uses a bitfield as it's underlying
 * storage.
 * 
 * @author Tobias.Gierke@code-sourcery.de
 */
class BitfieldTorus( private val data : BitfieldStorage  ) extends Torus[Boolean](data) {

	def this(width:Int,height:Int) {
		this( new BitfieldStorage(width,height) )
	}
	
	def createCopy() : BitfieldTorus = {
		new BitfieldTorus( data.createCopy() )
	}
	
	def visitAlive( func : => (Int,Int) => Unit ) {
		data.visitAlive( func )
	}
}

class BitfieldStorage(val w:Int,val h:Int) extends TwoDimensionalStorage[Boolean](w,h) {
	
	// number of bits per array element
	private val ARRAY_ELEMENT_BIT_WIDTH = 32
	
	// I use shift operations instead of divide / multiply
	private val SHIFT_BITCOUNT = 5 // = 2^5 = 32 
	
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
	
	def visitAlive( func : => (Int,Int) => Unit ) {
		
		var bitOffset = 0
		var index = 0
		val len = data.length
		
		while ( index < len ) {
			val value = data(index)
			if ( value != 0 ) 
			{ // at least one bit is set
				var currentOffset = bitOffset
				var currentBit = 0
				while ( currentBit < ARRAY_ELEMENT_BIT_WIDTH ) 
				{
					if ( ( value & ( 1 << currentBit ) ) != 0 ) {
						val y = currentOffset / paddedWidth
						val x = currentOffset % paddedWidth
						func( x , y )
					}
					currentBit += 1
					currentOffset += 1
				}
			}
			bitOffset += ARRAY_ELEMENT_BIT_WIDTH
			index += 1
		}
	}
	
	def clear() {
		data = new Array[Int]( calcLength(w,h ) )
	}
	
	def getValueAt(x:Int,y:Int) : Boolean = {
		
		val bitOffset : Int = x + y* paddedWidth
		val byteOffset : Int = bitOffset >>> SHIFT_BITCOUNT //   read: bitOffset / ARRAY_ELEMENT_BIT_WIDTH 
		val offsetInByte : Int = bitOffset - ( byteOffset  << SHIFT_BITCOUNT ) // read: byteOffset * ARRAY_ELEMENT_BIT_WIDTH )
		
		return ( data(byteOffset) & ( 1 << offsetInByte ) ) != 0
	}
	
	def setValueAt(x:Int,y:Int,value:Boolean) : Boolean = {
		
		val bitOffset : Int = x + y* paddedWidth
		val byteOffset : Int = bitOffset >>> SHIFT_BITCOUNT //   read: bitOffset / ARRAY_ELEMENT_BIT_WIDTH
		val offsetInByte : Int = bitOffset - ( byteOffset  << SHIFT_BITCOUNT ) // read: byteOffset * ARRAY_ELEMENT_BIT_WIDTH )
		
		val oldValue = ( data(byteOffset) & ( 1 << offsetInByte ) ) != 0
		
		if ( value ) { // set bit
			data(byteOffset) = ( data(byteOffset) | 1 << offsetInByte)
		} else { // clear bit
			data(byteOffset) = (data(byteOffset) & ~(1 << offsetInByte) )
		}
		oldValue
	}
	 
	def createCopy() : BitfieldStorage = {
	 		val result = new BitfieldStorage( w , h )
	 		Array.copy( data , 0 , result.data , 0 , data.length )
	 		result
	}
	
	def createCopy(newWidth:Int,newHeight:Int) : TwoDimensionalStorage[Boolean] = {

 		val result = new BitfieldStorage( newWidth , newHeight )

 		val copyFunction : (Int,Int,Boolean) => Unit = { (x,y,isSet) => {
 			if ( isSet && ( x < newWidth && y < newHeight ) ) {
 				result.setValueAt( x , y , true )
 			}
 		} }
 	    visitAll( copyFunction );
 		result
	}	
}