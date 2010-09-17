package de.codesourcery.life.ui

class TracingRepaintManager extends javax.swing.RepaintManager {

	   @Override
   override def addDirtyRegion( c : javax.swing.JComponent, x : Int, y:Int, w:Int, h:Int) 
   {
      try {
         throw new Exception();
      } catch {
      	case exc : Exception => {
         val sb = new StringBuffer();
         val stack = exc.getStackTrace();
         var count = 0;
         exc.printStackTrace()
         System.out.println("**** Repaint stack ****");
//         System.out.println(sb.toString());
      }
      }

      super.addDirtyRegion(c, x, y, w, h);
   }

}