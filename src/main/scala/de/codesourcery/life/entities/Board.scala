package de.codesourcery.life.entities

import scala.xml.Node
import scala.xml.Elem

/**
 * This is the simulation's data model.
 * 
 * <p>
 * This class implements the actual 'Game of Life'
 * algorithm on an (at least mathematically) 
 * infinite 'game board' and also holds the
 * simulation's data model (cell data and
 * a generation counter).</p>
 * 
 * <p>Internally, simulation data gets stored as 
 * boolean values (<code>true</code> = cell is alive)
 * in a two-dimensional array.</p>
 * <p>To simulate an infinite extend of the 
 * 'playing field' , the internal array
 * is treated as a torus (X/Y coordinates
 * get wrapped around when trying 
 * to access elements beyond the actual
 * array width / height).</p>
 *
 * @author tobias.gierke@code-sourcery.de
 */
class Board private (private var torus : Torus[Boolean] ) {

	/**
	 * Holds the simulation's current generation counter.
	 */
	private var currentGeneration = 1

	/**
	 * Creates a game board with a given
	 * size.
	 */
	def this( w:Int , h:Int) {
		this( Board.createTorus(w,h) )
	}
	
	/**
	 * Returns the actual extent (width) of the underlying
	 * data model.
	 * 
	 * @return
	 */
	def width : Int = torus.width
	
	/**
	 * Returns the actual extent (height) of the underlying
	 * data model.
	 * 
	 * @return
	 */	
	def height : Int = torus.height
	
	override def toString() : String = "Board[ "+width+" x "+height+" ]"
	
	/**
	 * Returns the simulation's current generation counter.
	 * @return
	 */	
	def generation : Int = currentGeneration
	
	/**
	 * Creates an independent copy
	 * of this simulation state.
	 * 
	 * @return
	 */
	def createCopy() : Board = {
		val result = new Board( torus.createCopy() )
		result.currentGeneration = currentGeneration
		result
	}
	
	def resize(newWidth:Int,newHeight:Int) {
		require( newWidth > 0 )
		require( newHeight > 0 )
		torus.resize( newWidth , newHeight )
	}
	
	def printBoard() {
		visitAll( { (x , y , isSet ) => {
			if ( x == 0 ) {
				println
			}
			if ( torus.get( x,y ) ) {
				print(" X |")
			} else {
				print("   |")
			}
		} } ) 
	}
	
	def printNeighbourCount() {
		// calculate neighbour count once 
		// for each field and store those
		// in a two-dimensional array
		val neighbourCount = new AnyTorus[Int]( width , height )
		
		val countFunction : (Int ,Int ,Boolean) => Unit = ( x , y , value ) => {
			neighbourCount.set( x , y , getNeighbourCount( x , y ) )
		}
		torus.visitAll( countFunction )
		
		neighbourCount.visitAll( { (x , y , count ) => {
			if ( x == 0 ) {
				println
			}
			print( count+"  " )
		} } ) 		
	}
	
	/**
	 * Populates THIS simulation state from
	 * another.
	 * 
	 * This instance will be come an
	 * independent copy of the source
	 * state.
	 * @param source
	 */
	def valueOf( source : Board) {
		torus = source.torus.createCopy
		currentGeneration = source.currentGeneration 
	}
	
	/**
	 * Resets the simulation to a state
	 * where all cells are dead.
	 * 
	 * The generation counter is reset to 1 as well.
	 */
	def reset() {
		visitAll( ( x , y , isSet ) => { torus.set( x,y,false ) } )
		currentGeneration = 1
	}
	
	/**
	 * Accepts a function that gets invoked once
	 * for each X-Y coordinate of the
	 * simulation model data.
	 * 
	 * @param func 
	 */
	def visitAll( func : => (Int,Int,Boolean) => Unit ) {
		torus.visitAll( (x : Int,y : Int,isSet : Boolean) => 
			func( x , y , isSet )
		)
	}
	
	/**
	 * Returns whether a cell is alive
	 * for a given X-Y coordinate.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	private def get(x:Int,y:Int) : Boolean = torus.get( x, y )
	
	/**
	 * Marks a cell at a given X-Y coordinate as dead.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	def clear(x : Int , y : Int) : Board = {
		torus.set(x,y,false)
		this
	}	
	
	/**
	 * Marks a cell at a given X-Y coordinate as being alive.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	def set(x : Int , y : Int) : Board = {
		torus.set(x,y,true)
		this
	}
	
	/**
	 * Returns whether the cell at a given X-Y
	 * coordinate is alive.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	def isAlive(x:Int,y:Int) : Boolean = torus.get( x , y)
	
	def getNeighbourCountMap() : Torus[Int] = {
		// calculate neighbour count once 
		// for each field and store those
		// in a two-dimensional array
		val neighbourCount = new AnyTorus[Int]( width , height )
		
		val countFunction : (Int ,Int ,Boolean) => Unit = ( x , y , value ) => {
			neighbourCount.set( x , y , getNeighbourCount( x , y ) )
		}
		torus.visitAll( countFunction )
		neighbourCount
	}
	
	/**
	 * Advances the simulation to the next generation.
	 * 
	 * @return <code>true</code> if the new population is different from the previous one
	 */
	def advance() : Boolean = {
		
		val neighbourCount = getNeighbourCountMap()
		
		// apply the rules to a new board
		val newBoard = Board.createTorus(width,height)
		
		var isStable = true // set to false if at least one cell dies or comes alive
		
		// hint: this function relies on
		// the fact that the new board is initially
		// clear (=no cells alive) and does
		// not flag 'dead' cells.
		val lifeFunction : ( Int , Int , Boolean) => Unit = (x,y,isAlive) => 
		{
			val count = neighbourCount.get( x ,y )
			
			if ( isAlive && ( count < 2 || count > 3 ) ) {
				newBoard.set( x , y , false ) // cell dies
				isStable = false
			} else if ( ! isAlive && count == 3 ) {
				newBoard.set( x , y , true ) // cell comes to life
				isStable = false
			} else if ( isAlive ) { // cell stays the same
				newBoard.set( x , y , true )
			}
			
		}
		
		torus.visitAll( lifeFunction )
		
		// advance to next generation
		torus = newBoard
		currentGeneration += 1
		
		return ! isStable
	}

	/**
	 * Returns the number of cells that are
	 * alive around a given X-Y coordinate.
	 * 
	 * <p>
	 * This method counts all cells within
	 * a 3x3 box centered at the given
	 * X-Y coordinate. 
	 * </p>
	 * @param x
	 * @param y
	 * @return
	 */
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
		
/*       Here we make use of the facts that we 
		
		 - know the iteration order ( row by row, starting
		   with the leftmost array element (zero) on each row)
		
		 - know that invalid array coordinates (less than zero
		   or greater than the actual width/height of
		   the underlying array) automatically get wrapped around
*/		
		torus.visit( x - 1 , y - 1 , x + 1  , y + 1 , f );
		return result
	}
	
}

object Board {
	
	/**
	 * Factory method.
	 * 
	 * @param width
	 * @param height
	 * @return
	 */
	def apply( width : Int , height : Int ) : Board = new Board( width , height )
	
	/**
	 * Saves simulation state to a XML file.
	 * 
	 * @param board
	 * @param file
	 */
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
	
	/**
	 * Loads simulation state from a XML file.
	 * 
	 * @param board
	 * @param file
	 */
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
	
	/**
	 * Factory method for testing different implementations
	 * of the underlying storage.
	 * 
	 * @param w
	 * @param h
	 * @return
	 */
	private def createTorus( w : Int  , h:Int) : Torus[Boolean] = {
//		new AnyTorus[Boolean](w,h)
		new BitfieldTorus(w,h)
	}
}
