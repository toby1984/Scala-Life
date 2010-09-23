package de.codesourcery.life

import de.codesourcery.life.entities._

object QuadTreeTest {

	private val tree = new QuadTree(512,512)
			
	class MyPanel extends javax.swing.JPanel {
		
		override def paint(graphics:java.awt.Graphics) 
		{
			super.paint(graphics)
			
			tree.visit( node => 
			{
				graphics.drawRect( node.envelope.x1 , node.envelope.y1 , node.envelope.width , node.envelope .height )

				node.value match {
					case Some(point) => {
						graphics.drawArc( point.x ,point.y , 3 , 3 , 0 , 360 )
					}
					case _ =>
				}
			} )
		}
		
		addMouseListener( new java.awt.event.MouseAdapter() {
			
			override def mouseClicked(ev:java.awt.event.MouseEvent) {
				println("Mouse clicked at "+ev.getX+" , "+ev.getY )
				tree.setValueAt( ev.getX , ev.getY , true )
				repaint()
			}
		} )
		
		setPreferredSize(new java.awt.Dimension( tree.width , tree.height ))
	}
	
	def main(args:Array[String]) {

		val frame = new javax.swing.JFrame("test")
		frame.setDefaultCloseOperation( javax.swing.JFrame.EXIT_ON_CLOSE )
		frame.getContentPane().add( new MyPanel() )
		frame.pack()
		frame.setVisible(true)
	}
}