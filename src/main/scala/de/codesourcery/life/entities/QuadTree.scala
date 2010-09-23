package de.codesourcery.life.entities

class QuadTree(private val w : Int, private val h : Int) extends TwoDimensionalStorage[Boolean](w,h) {

	private var root = new TreeNode( new Envelope(0,0,w,h ) )
	
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
		return root.getValue(x,y)
	}
	
	def print() {
		root.print()
	}
	
	def visit(func: => TreeNode => Unit ) {
		root.visit( func )
	}
	
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
	def setValueAt(x:Int,y:Int,value:Boolean)  : Boolean = {
		root.setValue(x,y,value)
	}
	
	private def contains(x:Int,y:Int) : Boolean = root.contains(x,y)
	
	/**
	 * Creates an independent copy of this
	 * instance.
	 * 
	 * @return
	 */
	def createCopy() : TwoDimensionalStorage[Boolean] = {
		val copy = new QuadTree( width , height )
		root.visit( node => { 
			val p = node.value 
			if ( p.isDefined ) {
				copy.setValueAt( p.get.x , p.get.y , true )
			}
		})
		copy
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
	def createCopy(width:Int,height:Int) : TwoDimensionalStorage[Boolean]	= {
		
		val copy : TwoDimensionalStorage[Boolean] = if ( QuadTree.isSupportedSize(width,height) ) new QuadTree(width,height) else new BitfieldStorage(width,height)
		
		def isValid(someX:Int,someY:Int) : Boolean = someX >= 0 && someX <= width && someY >= 0 && someY <= height
		
		root.visit( node => { 
			val p = node.value 
			if ( p.isDefined ) {
				val x = p.get.x
				val y = p.get.y 
				if ( isValid(x,y) ) {
					copy.setValueAt( x , y , true )
				}
			}
		})
		copy		
	}
	
    /**
	 * Resets this storage's content
	 * to it's initial state.
	 */
	def clear() {
		root = new TreeNode( new Envelope(0,0,w,h ) )
	}
}

case class Point(x:Int,y:Int) 
{
	def matches(someX:Int,someY:Int) = x == someX && someY == y
	
	override def toString = "Point[ "+x+","+y+" ]"
}

class TreeNode(val envelope : Envelope, var value: Option[Point]) {
	
	def this(envelope : Envelope) {
		this(envelope , None )
	}
	
	override def toString = "Node[ "+envelope+" , value="+value+"]"
	
	/*
	 * Quadrants:
	 *         |
	 *    I    |    II
	 * --------+---------
	 *   III   |    VI
	 *         |
	 */
	
	private val quadrants = new Array[TreeNode](4)
	
	private def getNode(index:Int) : TreeNode = {
		return quadrants(index)
	}
	
	def visit(func: => TreeNode => Unit ) {
		
		func( this )
		
		var index = 0
		while ( index < 4 ) {
			if ( quadrants(index) != null ) {
				quadrants(index).visit( func )
			}
			index+=1
		}	
	}
	
	def print() {
		println( this )
		var index = 0
		while ( index < 4 ) {
			if ( quadrants(index) != null ) {
				quadrants(index).print()
			}
			index+=1
		}	
	}
	
	def contains(x:Int,y:Int) : Boolean = {
		return envelope.contains(x,y)
	}
	
	private def createQuadrant(currentPoint:Point, quadrant:Int) : Envelope = quadrant match {
		case 0 => new Envelope( envelope.x1 , envelope.y1 , currentPoint )
		case 1 => new Envelope( currentPoint.x , envelope.y1 , envelope.x2 , currentPoint.y ) 
		case 2 => new Envelope( envelope.x1 , currentPoint.y , currentPoint.x , envelope.y2 )
		case 3 => new Envelope( currentPoint.x , currentPoint.y , envelope.x2 , envelope.y2 )
	}
	
	def getValue(x:Int,y:Int) : Boolean = {
		
		if ( value.isDefined ) 
		{
			if ( value.get.matches( x , y) ) {
				return true
			}
		}
		
		var index = 0
		while ( index < 4 ) {
			if ( quadrants(index) != null && quadrants(index).envelope .contains(x,y) ) {
				return quadrants(index).getValue( x,y )
			}
			index+=1
		}		
		return false
	}
	
	def setValue(x:Int,y:Int,newValue:Boolean) : Boolean = {
		
		require( envelope.contains(x, y) )
	
		if ( ! value.isDefined ) 
		{
			// no value set in this quadrant
			if ( newValue == true ) { 
				value = Some(Point(x,y))
			} // nothing to do since false == None
			return false			
		}
		
		// a value is already present on this node
		val currentPoint = value.get
		if ( currentPoint.matches(x,y) )  
		{
			if ( newValue == true ) {
				return true // point exists => is set
			}
			// clear point
			value = None
			return true
		}
		
		// we reached the max. capacity for this node 
		var matchingQuadrant : TreeNode = null
		var index = 0
		while ( index < 4 ) {
			if ( quadrants(index) == null ) {
				val env = createQuadrant(currentPoint,index)
				if ( env.contains(x,y ) ) {
					val newNode = new TreeNode( env )
					quadrants(index) = newNode
					newNode.setValue(x,y,newValue)
					return false
				}
			} else if ( quadrants(index).envelope.contains( x,y ) ) {
				return quadrants(index).setValue(x,y,newValue)
			}
			index+=1
		}

		throw new RuntimeException("Unreachable code reached")
	}
}

class Envelope(val x1 : Int, val y1 : Int, val x2 : Int, val y2 : Int)
{
        def this(x1 : Int, y1 : Int,p:Point) {
                this( x1 , y1 , p.x , p.y )
        }

        def contains(x : Int , y : Int ) : Boolean = {
                return ( x >= x1 && x <= x2 ) && ( y >= y1 && y <= y2 );
        }

        def width = ( x2 - x1 )
        def height = ( y2 - y1 )

        override def toString = "Envelope[ ("+x1+","+y1+") , ("+x2+","+y2+") ]"
}

object QuadTree {
	
	private def log2(v:Int) : Double = Math.log( v ) / Math.log(2)
	
	private def isPowerOfTwo(v:Int) : Boolean = {
		val log = log2( v )
		log == Math.abs( log )
	}
	
	def isSupportedSize(w:Int,h:Int) : Boolean = isPowerOfTwo(w) && isPowerOfTwo(h)
}