package de.codesourcery.life.ui

import javax.swing.filechooser.FileFilter
import de.codesourcery.life.entities.Board
import java.awt.FlowLayout
import java.awt.BorderLayout
import java.awt.event._
import javax.swing.JFileChooser
import javax.swing.SwingUtilities
import javax.swing.JButton
import javax.swing.JPanel
import javax.swing.JSlider
import javax.swing.event.{ChangeListener,ChangeEvent}

class LifeFrame extends javax.swing.JFrame("Generation 1") with View {

	private[this] var controller : Option[UIController]=None 
	
	def setController(controller : UIController ) {
		require( controller != null )
		this.controller = Some(controller)
		updateSimulatorSpeed( controller.getSimulationDelayInMillis )
	}
	
	private val speedDial = new JSlider(0,100,50) {
		addChangeListener( new ChangeListener() { 
			def stateChanged(ev:ChangeEvent) {
				controller.foreach( _.speedChanged( getValue() ) )
			}
		})
	}
	
	private def model : Board = controller.get.model
	
	private val drawPanel = new JPanel() 
	{
		private val CANVAS_WIDTH = 320
		private val CANVAS_HEIGHT = 240
		
		private val someMouseListener : java.awt.event.MouseAdapter = new java.awt.event.MouseAdapter() 
		{
			var lastCoords : Option[(Int,Int)] = None
			
			override def mouseReleased(ev : java.awt.event.MouseEvent) {
				lastCoords = None
			}
			
			override def mouseDragged(ev : java.awt.event.MouseEvent) {
				
				val currentCoords = getModelCoordsForPoint( model , ev.getX , ev.getY )
				if ( ! lastCoords.isDefined || lastCoords.get != currentCoords ) {
					controller.get.cellClicked( currentCoords._1 , currentCoords._2 )
					lastCoords = Some( currentCoords )
				}
			}
			
			override def mouseClicked( ev : java.awt.event.MouseEvent ) {
				if ( ev.getClickCount == 1 ) {
					controller match {
						case Some(ctrl) => {
							val coords = getModelCoordsForPoint( model , ev.getX , ev.getY )						
							ctrl.cellClicked(  coords._1 , coords._2 )
						}
						case _ =>
					}
				}
			}
		}		
		
		private def getModelCoordsForPoint( board: Board, x : Int , y : Int ) : (Int,Int) = {
			var maxX  = getWidth
			var maxY = getHeight 
		
			val stepX : Int = Math.floor( maxX / board.width ).asInstanceOf[Int]
		    val stepY : Int = Math.floor( maxY / board.height ).asInstanceOf[Int]
            ( x / stepX , y / stepY )
	    }
	
		private[this] val paintFunction : java.awt.Graphics => Unit =  graphics => 
		{
			var maxX  = getWidth
			var maxY = getHeight 
			
			val board = model
			
			val stepX : Int = Math.floor( maxX / board.width ).asInstanceOf[Int]
			val stepY : Int = Math.floor( maxY / board.height ).asInstanceOf[Int]
			
			maxX = stepX * board.width
			maxY = stepY * board.height
			
			// draw grid
			graphics.setColor( java.awt.Color.black )
			
			var x : Int = 0;
			while( x <= maxX ) {
				graphics.drawLine( x , 0 , x  , maxY )
				x += stepX
			}
			
			var y : Int = 0;
			while( y <= maxY) {
				graphics.drawLine( 0 , y , maxX , y )
				y += stepY
			}
			
			// render board
			val drawFunction : (Int,Int,Boolean) => Unit = {
				( x , y , isSet ) => {
					if ( isSet ) {
						val x1 : Int = x * stepX
						val y1 : Int = y * stepY
						graphics.fillRect( 
								x1+2 , 
								y1+2,
								stepX-3 , 
								stepY-3 )
					}
				}
			}
			board.visitAll( drawFunction )			
		}
	
		override def paint( graphics : java.awt.Graphics ) {
			
			val old1 = getBackground()
			val old2 = getForeground()
			
			setForeground( java.awt.Color.WHITE )
			setBackground( java.awt.Color.WHITE )
			super.paint(graphics )
			
			setForeground( old2 )
			setBackground( old1 )
			
			if ( controller.isDefined ) {
				paintFunction( graphics )
			} 
		}	
		
		// drawPanel constructor
		addMouseListener( someMouseListener )
		addMouseMotionListener( someMouseListener )
		
		val mySize = new java.awt.Dimension( CANVAS_WIDTH, CANVAS_HEIGHT ) 
		setPreferredSize( mySize )
	}
	
	private def round(f :Float) = Math.round(f )
	
	private[this] val simulatorDelay = new javax.swing.JTextField() {
		setEditable( false )
	}
	
	private[this] val loadFromFileButton = new JButton("Load...")
	private[this] val saveToFileButton = new JButton("Save...")
	
	private[this] val clearButton = new JButton("Clear")
	private[this] val startButton = new JButton("Start...")
	private[this] val stopButton = new JButton("Stop") {
		setEnabled( false )
	}
	
	private[this] val saveStateButton = new JButton("Save state")
	private[this] val recallStateButton = new JButton("Recall state") {
		setEnabled( false )
	}
	
	private[this] val resetButton = new JButton("Reset")
	
	def savedStateAvailable() {
		recallStateButton.setEnabled( true )
	}
	
	private def updateSimulatorSpeed( delayInMillis : Int ) {
		simulatorDelay.setText( delayInMillis+" ms" )		
	}
	
	def simulatorSpeedChanged( delayInMillis : Int ) {
		updateSimulatorSpeed( delayInMillis )
	}
	
	def simulatorStateChanged(isRunning:Boolean) {
		clearButton.setEnabled( ! isRunning )
		stopButton.setEnabled( isRunning )
		startButton.setEnabled( ! isRunning )
		controlPanel.repaint()
	}
	
	private def showFileChooser( showSaveDialog : Boolean ) : Option[String] = {
		val chooser = new javax.swing.JFileChooser()
		
		chooser.setFileFilter( new FileFilter() {
			def accept(file:java.io.File) : Boolean = {
				file.isDirectory() || ( file.isFile() && file.getName().endsWith(".life.xml") )
			}
			
			def getDescription() : String = "*.life.xml"
		} )
		
		val retVal = if ( showSaveDialog) {
			chooser.showSaveDialog( LifeFrame.this ) 
		} else {
			chooser.showOpenDialog( LifeFrame.this ) 
		}
		
		if ( retVal == JFileChooser.APPROVE_OPTION) {
			val path = chooser.getSelectedFile().getAbsolutePath()
			if ( path.endsWith(".life.xml" ) ) {
				Some( path )
			} else {
				Some( path+".life.xml" )
			}
		} else {
			None
		}
	}
	
	def querySaveFileName() : Option[String] = showFileChooser( true )
		
	def queryLoadFileName() : Option[String] = showFileChooser( false )
	
	private val controlPanel = new JPanel() {
		
		private val listener = new ActionListener() {
			
			def actionPerformed(ev:ActionEvent) {
				
				if ( ! controller.isDefined ) 
				{
					return
				}
				
				val button = ev.getSource().asInstanceOf[JButton]
				if ( button == startButton ) {
					controller.get.startButtonClicked()
				} else if ( button == stopButton ) {
					controller.get.stopButtonClicked()
				} else if ( button == clearButton ) {
					controller.get.clearButtonClicked()
				} else if ( button == resetButton ) {
					controller.get.resetButtonClicked()
				} else if ( button == saveStateButton ) {
					controller.get.saveStateClicked()
				} else if ( button == recallStateButton ) {
					controller.get.recallStateClicked()
				} else if ( button == loadFromFileButton) {
					controller.get.loadStateFromFileClicked()
				} else if ( button == saveToFileButton) {
					controller.get.saveStateToFileClicked()
				}				
			}
		}
		
		setLayout(new FlowLayout() )
		
		startButton.addActionListener( listener )
		stopButton.addActionListener( listener )
		clearButton.addActionListener( listener )
		resetButton.addActionListener( listener )
		saveStateButton.addActionListener( listener )
		recallStateButton.addActionListener( listener )		
		loadFromFileButton.addActionListener( listener )
		saveToFileButton.addActionListener( listener )
		
		add( new javax.swing.JLabel( "Simulation delay" ) )
		add( simulatorDelay )
		add( speedDial )
		add( startButton )
		add( stopButton )
		add( clearButton )
		add( resetButton )
		add( saveStateButton )
		add( recallStateButton )
		add( loadFromFileButton )
		add( saveToFileButton )
	}
	
	def modelChanged() {
		if ( controller.isDefined ) {
			setTitle( "Generation "+model.generation )
		}
		drawPanel.repaint()
	}
	
	setDefaultCloseOperation(  javax.swing.JFrame.EXIT_ON_CLOSE )
	
	private[this] val wrapper = new JPanel
	wrapper.setLayout( new BorderLayout() )
	wrapper.add( controlPanel , BorderLayout.NORTH )
	wrapper.add( drawPanel , BorderLayout.CENTER )
	getContentPane().add( wrapper )
	pack()
}
