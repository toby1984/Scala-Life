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
	
	// bit-mask with only 1s (length must match ARRAY_ELEMENT_BIT_WIDTH)
	private val ALL_ONES_MASK = 0xffffffff
	
	private val paddedWidth : Int =  if ( ( w % ARRAY_ELEMENT_BIT_WIDTH ) != 0 ) {
		val tmp :Int = Math.ceil( w.asInstanceOf[Float] / ARRAY_ELEMENT_BIT_WIDTH.asInstanceOf[Float] ).asInstanceOf[Int]
		tmp * ARRAY_ELEMENT_BIT_WIDTH
	} else {
		height 
	}
	
	private var data : Array[Int]= new Array[Int]( calcLength(w,h ) )
	
	def calcLength(aWidth : Int, aHeight : Int ) : Int = {
		Math.ceil( ( aHeight * paddedWidth ) / ARRAY_ELEMENT_BIT_WIDTH ).asInstanceOf[Int]
	}
	
	def clear() {
		data = new Array[Int]( calcLength(w,h ) )
	}
	
	private def calcArrayPosition(x:Int,y:Int) : (Int,Int) = {
		val bitOffset : Int = x + y* paddedWidth
		val byteOffset : Int = bitOffset / ARRAY_ELEMENT_BIT_WIDTH 
		val offsetInByte : Int = bitOffset -( byteOffset * ARRAY_ELEMENT_BIT_WIDTH )
		return ( byteOffset , offsetInByte )
	}
	
	def getValueAt(x:Int,y:Int) : Boolean = {
		
		val ( byteOffset ,offsetInByte ) = calcArrayPosition(x,y)
		try {
			return ( data(byteOffset) & 1 << offsetInByte ) != 0
		} catch {
			case ex : ArrayIndexOutOfBoundsException => {
				throw new IllegalArgumentException("\nArray: "+w+"x"+h+" of len "+data.length+"\nInvalid get("+x+" / "+y+") access at "+
						" byteoffset "+byteOffset+", bit = "+offsetInByte)
			}
			case x: Throwable => throw x 
		}
	}
	
	def setValueAt(x:Int,y:Int,value:Boolean) {
		
		val ( byteOffset ,offsetInByte ) = calcArrayPosition(x,y)
				
		if ( value ) { // set bit
			data(byteOffset) = ( data(byteOffset) | 1 << offsetInByte)
		} else { // clear bit
			val mask = ALL_ONES_MASK - ( 1 << offsetInByte )
			data(byteOffset) = (data(byteOffset) & mask)
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