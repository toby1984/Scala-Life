package de.codesourcery.life

import de.codesourcery.life.entities.Board
import de.codesourcery.life.ui.UIController
import de.codesourcery.life.ui.LifeFrame
import javax.swing.SwingUtilities

object Main{

	private def initModel(board:Board) : Board = {
		board.reset()
		board.set( 2 , 1 ).set( 1, 3 ).set( 2 , 3 ).set( 4 , 2 ).set( 5 , 3 ).set( 6 , 3 ).set( 7 , 3 )
		board
	}
	
	def main( args : Array[String] ) 
	{
			val model = initModel( Board( 100 , 100 ) )
			
//			var time = -System.currentTimeMillis
//			var i = 1600
//			while ( i > 0 ) {
//				model.advance
//				i-=1
//			}
//			time += System.currentTimeMillis
//			println("Time: "+time)
			
			val view = new LifeFrame()
			view.setVisible( true )
			
			val controller = new UIController( model , view ) 
			{
				protected def initModel(b:Board) { Main.initModel( b ) }
			}
	}
	
}