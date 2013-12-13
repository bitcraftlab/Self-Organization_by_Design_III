
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

// reserve a bit of space for text
int top = 20;

void setup() {

  // create a processing sketch
  size(500, 500 + top);
  textAlign(RIGHT);
  colorMode(HSB);
  
  // load netlogo model from the data folder
  nl = new NetLogo(this, "Fire.nlogo");
  
  reset();
  
}


void reset() {
  
  // clear the screen
  background(0);
  
  // setup the model
  nl.cmd("set density 62");
  nl.cmd("random-seed 0");
  nl.cmd("setup");
  
}

void draw() {

  // report the number of burned trees
  int result = (int) nl.number("burned-trees");
  
  // show the result on screen
  fill(0); rect(0, 0, width, 20);
  fill(100); text("burned trees: ", width - 50, 17);
  text(result, width - 10, 17);
  
  // fade to black
  noStroke();
  fill(0, 10);
  rect(0, top, width, height);
  
  // trigger two steps in your starlogo simulation
  nl.cmd("repeat 2 [go]");
  
  // translate to center
  translate(width/2, height/2 + top);
  
  // double rainbow all the way
  fill(frameCount % 255, 127, 255);

  // get a list of fires
  ArrayList<Agent> fires = nl.agents("fires");
    
  for(Agent fire : fires) {
    
    // get variable values by position (That's kind of ugly - planning to add a processing wrapper ...)
    int id = int(fire.getVariable(0).toString()); 
    int xcor = int(fire.getVariable(3).toString());
    int ycor = int(fire.getVariable(4).toString());
   
    // draw something ath the turtle positon
    ellipse(2 * xcor, 2 * ycor, 8, 8);
    
  }
  
}

// restart the simulation by pressing a key
void keyPressed() {
  reset(); 
}

