package xyz.hyperreal.chess


import scala.swing._

object Main extends SimpleSwingApplication {
	val board = new Board
	val panel = new BoardPanel( board )

	lazy val top = new MainFrame() {
		title = "Chess"
		contents = new BorderPanel {
			layout(panel) = BorderPanel.Position.Center
			layout(new FlowPanel(
				Button( "Restart" ) {
					board.init
					panel.repaint
				},
				Button( "Switch" ) {
					panel.switch
				},
				Button( "Undo" ) {
					if (board.canUndo) {
						board.undo
						panel.repaint
					}
				}
			)) = BorderPanel.Position.South
		}
	}
}