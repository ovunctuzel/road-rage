// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.event.{EventHandler, ActionEvent}
import javafx.scene.input.{MouseEvent, ScrollEvent, MouseButton}
import javafx.util.Duration
import javafx.animation.{Timeline, KeyFrame, ScaleTransition, Animation}

import utexas.aorta.Util

// TODO scalafx looks darn cool...
// http://code.google.com/p/scalafx/


// TODO
// - zooming that goes in the right place
// - decent rotation
// - smooth zooming and panning


object GUI {
  def main(args: Array[String]) = {
    val sim = Util.process_args(args)
    Content.setup(sim)
    Application.launch(classOf[GUI], args: _*)
  }
}

class GUI() extends Application {
  // Global state about what we're doing now
  private var x_off = 0.0
  private var y_off = 0.0
  private var rot_degrees = 0.0
  private var zoom = 1.0

  // Temporary state about what we're doing with the mouse
  private var click_is_pan = false
  private var click_x = 0.0 // TODO None when not in use?
  private var click_y = 0.0
  private var rot_base = 0.0

  override def start(stage: Stage): Unit = {
    val root = Content.geometry_root

    val scene = new Scene(root, 800, 600, Color.GRAY)
    scene.setOnMousePressed(new EventHandler[MouseEvent]() {
      def handle(event: MouseEvent) = {
        // Start a pan or rotate
        if (event.getButton != MouseButton.MIDDLE) {
          click_x = event.getX
          click_y = event.getY
          click_is_pan = event.getButton == MouseButton.PRIMARY
          rot_base = rot_degrees
        }
      }
    })
    /*scene.setOnMouseReleased(new EventHandler[MouseEvent]() {
      def handle(event: MouseEvent) = {
        println("stopping drag or rotate")
        click_x = 0.0
        click_y = 0.0
      }
    })*/
    scene.setOnMouseDragged(new EventHandler[MouseEvent]() {
      def handle(event: MouseEvent) = {
        // Pan
        if (click_is_pan) {
          // Dragging down means pan up
          val dx = event.getX - click_x
          val dy = event.getY - click_y
          x_off += dx
          y_off += dy
          root.setTranslateX(x_off)
          root.setTranslateY(y_off)
          // Reset these so the next drag event works fine
          click_x = event.getX
          click_y = event.getY
        } else {
          // We don't get drag events for the middle click, so assume we're
          // rotating
          val rot_delta = math.toDegrees(
            math.atan2(event.getX - click_x, event.getY - click_y)
          )
          rot_degrees = rot_base - rot_delta
          root.setRotate(rot_degrees)
        }
      }
    })
    scene.setOnScroll(new EventHandler[ScrollEvent]() {
      def handle(event: ScrollEvent) = {
        // TODO assume only one increment?
        if (event.getDeltaY > 0) {
          zoom += 0.1
        } else {
          zoom -= 0.1
        }
        /*val transition = new ScaleTransition(
          Duration.millis(500), root
        )
        transition.setToX(zoom)
        transition.setToY(zoom)
        transition.play()*/
        root.setScaleX(zoom)
        root.setScaleY(zoom)
      }
    })

    // Make the simulation run
    // TODO this still sleeps and doesnt let ya control speed
    val run_sim = new Timeline(
      new KeyFrame(Duration.millis(1000), new EventHandler[ActionEvent]() {
        def handle(event: ActionEvent) = {
          Content.content.step()
        }
      })
    )
    run_sim.setCycleCount(Animation.INDEFINITE)
    run_sim.play()

    stage.setTitle("AORTA GUI")
    stage.setScene(scene)
    stage.show()
  }
}
