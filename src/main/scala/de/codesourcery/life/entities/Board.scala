package de.codesourcery.life.entities

import scala.xml.Node
import scala.xml.Elem

class Board private (private var torus : Torus[Boolean] ) {

	private var currentGeneration = 1
	
	def this( w:Int , h:Int) {
		this( Board.createTorus(w,h) )
	}
	
	def width : Int = torus.width
	def height : Int = torus.height
	
	override def toString() : String = "Board[ "+width+" x "+height+" ]"
	
	def generation : Int = currentGeneration
	
	def createCopy() : Board = {
		val result = new Board( torus.createCopy() )
		result.currentGeneration = currentGeneration
		result
	}
	
	def valueOf( b : Board) {
		torus = b.torus.createCopy
		currentGeneration = b.currentGeneration 
	}
	
	def reset() {
		visitAll( ( x , y , isSet ) => { torus.set( x,y,false ) } )
		currentGeneration = 1
	}
	
	def visitAll( func : => (Int,Int,Boolean) => Unit ) {
		torus.visitAll( (x : Int,y : Int,isSet : Boolean) => 
			func( x , y , isSet )
		)
	}
	
	def get(x:Int,y:Int) : Boolean = torus.get( x, y )
	
	def clear(x : Int , y : Int) : Board = {
		torus.set(x,y,false)
		this
	}	
	
	def set(x : Int , y : Int) : Board = {
		torus.set(x,y,true)
		this
	}
	
	def isAlive(x:Int,y:Int) : Boolean = torus.get( x , y)
	
	/**
	 * 
	 * @return <code>true</code> if new generation is different from the current one
	 */
	def advance() : Boolean = {
		
		// calculate neighbour count once 
		// for each field
		val neighbourCount = new AnyTorus[Int]( width , height )
		
		val countFunction : (Int ,Int ,Boolean) => Unit = ( x , y , value ) => {
			neighbourCount.set( x , y , getNeighbourCount( x , y ) )
		}
		torus.visitAll( countFunction )
		
		// apply the rules to a new board
		val newBoard = Board.createTorus(width,height)
		
		var isStable = true 
		
		val lifeFunction : ( Int , Int , Boolean) => Unit = (x,y,isAlive) => 
		{
			val count = neighbourCount.get( x ,y )
			
			if ( isAlive && ( count < 2 || count > 3 ) ) {
				newBoard.set( x , y , false ) // cell dies
				isStable = false
			} else if ( ! isAlive && count == 3 ) {
				newBoard.set( x , y , true ) // cell comes to life
				isStable = false
			} else if ( isAlive ){
				newBoard.set( x , y , true )
			}
			
		}
		
		torus.visitAll( lifeFunction )
		
		// advance by one generation
		torus = newBoard
		currentGeneration += 1
		
		return ! isStable
	}

	private def getNeighbourCount(x:Int,y:Int) : Int = {
		
		var result = 0;
		val f : (Int,Int,Boolean) => Unit = {
			( currentX , currentY , set ) => 
			{
				if ( set && ( currentX != x || currentY != y ) ) {
					result += 1
				}
			}
		}
		
		torus.visit( x - 1 , y - 1 , x + 1  , y + 1 , f );
		return result
	}
	
}

object Board {
	
	def apply( width : Int , height : Int ) : Board = new Board( width , height )
	
	private def add(n:Node,c:Node):Node = n match { case e:Elem => e.copy(child=e.child++c) }
	
	def saveToFile(board:Board , file : String ) {
		
		var cells : scala.xml.NodeSeq = null
			
		board.visitAll( (x:Int,y:Int,isSet:Boolean) => {
			if ( isSet ) {
				if ( cells == null ) {
					cells = <cell x={x.toString} y={y.toString}/>
				} else {
					cells = cells ++ <cell x={x.toString} y={y.toString}/>
				}
			}
		} )
		
		val node = <life>
                     <width>{board.width}</width>
                     <height>{board.height}</height>
                     <generation>{board.generation}</generation>
                     <cells>{cells}</cells>
                   </life>
		
		scala.xml.XML.saveFull( file , node , "UTF-8" , true , null )
	}
	
	def populateFromFile(board:Board , file:String) {
		
		val data = scala.xml.XML.loadFile( file )
		
		val w = ( data \\ "width" ).text.toInt
		val h = ( data \\ "height" ).text.toInt
		val generation = ( data \\ "generation" ).text.toInt
		
		val tmp = new Board( w , h )
		tmp.currentGeneration = generation
		
		val aliveCells = ( data \\ "cell" )
		for ( cell <- aliveCells ) {
			val x = ( cell \ "@x" ).text.toInt
			val y = ( cell \ "@y" ).text.toInt
			tmp.set(x, y)
		}
		board.torus.setData( tmp.torus.getData() )
		board.currentGeneration = tmp.currentGeneration
	}
	
	private def createTorus( w : Int  , h:Int) : Torus[Boolean] = {
//		new AnyTorus[Boolean](w,h)
		new BitfieldTorus(w,h)
	}
}
