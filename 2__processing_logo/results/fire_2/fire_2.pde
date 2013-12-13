
import peasy.*;


//////////////////////////////////////////
//                                      //
//      F I R E     E X A M P L E       //
//                                      //
//////////////////////////////////////////

// A Simple Example for the Logo Library
// Martin Schneider @bitcraftlab 2013

// import the library
import bitcraftlab.logo.NetLogo;

// use the NetLogo API to access agents
import org.nlogo.api.Agent;
NetLogo nl;
PeasyCam cam;

// reserve a bit of space for text
int top = 20;
int density = 62;

void setup() {

  // create a processing sketch
  size(500, 500 + top, P3D);
  textAlign(RIGHT);
  colorMode(HSB);
  
  // load netlogo model from the data folder
  nl = new NetLogo(this, "Fire.nlogo");
  cam = new PeasyCam(this, width);
  
  reset();
  
}


void reset() {

  // setup the model
  nl.cmd("set density " + density);
  nl.cmd("random-seed 0");
  nl.cmd("setup");
  
}

void draw() {

  background(255);
  
  // report the number of burned trees
  int result = (int) nl.number("burned-trees");
  
  // show the result on screen
  cam.beginHUD();
  fill(0); rect(0, 0, width, 20);
  fill(100); text("burned trees: ", width - 50, 17);
  text(result, width - 10, 17);
  cam.endHUD();
  
  // fade to black
  /*
  noStroke();
  fill(0, 10);
  rect(0, top, width, height);
  */
  
  // trigger two steps in your starlogo simulation
  nl.cmd("repeat 2 [go]");
  
  // translate to center
  //translate(width/2, height/2 + top);
  

  // get a list of fires
  ArrayList<Agent> fires = nl.agents("fires");
    
  for(Agent fire : fires) {
    
    // get variable values by position (That's kind of ugly - planning to add a processing wrapper ...)
    int id = int(fire.getVariable(0).toString()); 
    int xcor = int(fire.getVariable(3).toString());
    int ycor = int(fire.getVariable(4).toString());
   
    // draw something ath the turtle positon
    
    // double rainbow all the way
    fill(frameCount % 255, 127, 255);
    pushMatrix();
    translate(2 * xcor, 2 * ycor, 50 * ( 1 + sin(frameCount * 0.2)));
    ellipse(0, 0, 20, 20);
    popMatrix();
    
    pushMatrix();
    fill(50);
    noStroke();
    translate(2 * xcor, 2 * ycor, 0);
    ellipse(0, 0, 20, 20);
    popMatrix();
    
  }
  
}

// restart the simulation by pressing a key
void keyPressed() {
  reset(); 
}

