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
	
	private var data = {
		val len = calcLength(w,h )
		println("Created array with len "+len)
		new Array[Byte]( len )
	}
	
	def calcLength(aWidth : Int, aHeight : Int ) : Int = {
		val paddedHeight :Int = (Math.ceil( aHeight / 8.0 ) * 8.0).asInstanceOf[Int]
		val result : Int = ( ( aWidth * paddedHeight ) / 8 ) + 2
		println("Bitfield length: "+result+" ("+aWidth+" x "+aHeight+" )")
		result
	}
	
	def getValueAt(x:Int,y:Int) : Boolean = {
		val bitOffset : Int = y*h + x
		val byteOffset : Int = bitOffset / 8 
		val offsetInByte : Int = bitOffset -( byteOffset << 3 )
		try {
			return ( data(byteOffset) & 1 << offsetInByte ) != 0
		} catch {
			case ex : ArrayIndexOutOfBoundsException => {
				throw new IllegalArgumentException("Invalid get() access at "+x+" , "+y+
						"( byteoffset "+byteOffset+", bitoffset "+bitOffset+", bit = "+offsetInByte+" )")
			}
			case x: Throwable => throw x 
		}
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
	 		Array.copy( data , 0 , result.data , 0 , data.length )
	 		result
	}
	
	def createCopy(newWidth:Int,newHeight:Int) : TwoDimensionalStorage[Boolean] = {
		println("createCopy( "+newWidth+","+newHeight+")")
	 		val result = new BitfieldStorage( newWidth , newHeight )
	 		val newLength = calcLength(newWidth,newHeight)
	 		val maxLength = if ( data.length <= newLength ) data.length else newLength  
	 		Array.copy( data , 0 , result.data , 0 , maxLength )
	 		result
	}	
}