package de.codesourcery.life

import de.codesourcery.life.entities.Board
import de.codesourcery.life.ui.UIController
import de.codesourcery.life.ui.LifeFrame
import de.codesourcery.life.ui.TracingRepaintManager
import javax.swing.SwingUtilities

/** 
 * Application entry point (commandline class).
 *  
 * @author Tobias.Gierke@code-sourcery.de
 */
object Main{

	private def initModel(board:Board) : Board = {
		board.reset()
		board.set( 2 , 1 ).set( 1, 3 ).set( 2 , 3 ).set( 4 , 2 ).set( 5 , 3 ).set( 6 , 3 ).set( 7 , 3 )
		board
	}
	
	def main( args : Array[String] ) 
	{
		val board2 = Board(10,10)
		board2.debug = true
		board2.set( 2 , 1 )
		board2.set( 1, 3 ).set( 2 , 3 ).set( 4 , 2 ).set( 5 , 3 ).set( 6 , 3 ).set( 7 , 3 )
		System.exit(0);
				
//		javax.swing.RepaintManager.setCurrentManager( new TracingRepaintManager() )
//		
//		java.awt.Toolkit.getDefaultToolkit().addAWTEventListener(  new java.awt.event.AWTEventListener() {
//			def eventDispatched( ev : java.awt.AWTEvent ) {
//				println("Received "+ev)
//			}
//		} 
//		, 0xffffffffffffffffL | java.awt.AWTEvent.INVOCATION_EVENT_MASK );
		
		val model = initModel( Board( 100 , 100 ) )
			
//		var time = -System.currentTimeMillis
//		var i = 1600
//		while ( i > 0 ) {
//			model.advance
//			i-=1
//		}
//		time += System.currentTimeMillis
//		println("Time: "+time)
			
			val view = new LifeFrame()
			view.setVisible( true )
			
			val controller = new UIController( model , view ) 
			{
				protected def initModel(b:Board) { Main.initModel( b ) }
			}
	}
	
}