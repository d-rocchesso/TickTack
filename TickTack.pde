import themidibus.*; 
import processing.sound.*;
SoundFile l, r, x, u, uu, tapL, tapR, tapX;
MidiBus myBus; // The MidiBus

final int MENUSCREEN = 1;
final int GAMESCREEN = 0;
final int decimation = 17;
int screenState = MENUSCREEN;
int sequenceLength = 2;
boolean recording = false;
boolean following = false;

final float SPEEDMUL = 100; // visual speed multiplier
final int PAUSE = 100; // pause between triplets
final int PERSISTENCE = 5; // persistence of visual beats
final int NUM = 200; // length of tail
float scale ;
final int WIDTH = 2560;
final int HEIGHT = 1200;

int frameNum = 0;
int fc1, fc2, fc3;
int fcv1, fcv2, fcv3;
int prev;
float mx[] = new float[NUM];
float my[] = new float[NUM];
// float vx, vy;
int inst, inst_1;
float ioi[] = new float[3];
float ioiH[] = new float[3];
float ioiV[] = new float[3];
int t;
char tap[] = new char[5]; // max 3 taps + 2 dummy taps
float lamp[] = {1.0, 1.0, 1.0}; // left and right amplitudes
float ramp[] = {1.0, 1.0, 1.0};  
float xamp[] = {1.0, 1.0, 1.0}; // down and up amplitudes
float uuamp[] = {1.0, 1.0, 1.0};  
boolean pressed = false;
float speedH=10000;
float speedV=10000;
int triggerH = millis();
int triggerBeatH = millis();
int triggerBeatH1 = millis();
int triggerBeatH2 = millis();
int triggerBeatV = millis();
int triggerBeatV1 = millis();
int triggerBeatV2 = millis();
int triggerV = millis();
int countH = 0;
int countV = 0;

Table table;
Table ghostTable;

void setup() {
  fullScreen();
  // size(2560,1200);
  // size(1920,1080);
  Sound.list();
  // Sound s = new Sound(this);
  // s.outputDevice(11); 
  MidiBus.list(); // List all available Midi devices on STDOUT. This will show each device's index and name.
  myBus = new MidiBus(this, "Xjam", "Real Time Sequencer");
  scale = float(width)/WIDTH; 
  print("scale = " + scale + "\n");
  noStroke();
  fill(255, 153);
  l = new SoundFile(this, "l.wav");
  r = new SoundFile(this, "r.wav");
  x = new SoundFile(this, "xx.wav");
  u = new SoundFile(this, "u.wav");
  uu = new SoundFile(this, "uuu.wav");
  tapL = new SoundFile(this, "bottone1L.wav");
  tapR = new SoundFile(this, "bottone1R.wav");  
  tapX = new SoundFile(this, "bottoneLR.wav");
  l.play();
  delay(300);
  r.play();
  delay(300);
  x.play();
  delay(300);
  u.play();
  delay(300);
  uu.play();
  textSize(20);
  ghostTable = loadTable("usa_2013.csv", "header");
  println(ghostTable.getRowCount() + " total rows in table");

}

void drawMenu(){
  background(100);
  text("Control the trajectory with the left and right arrows", 40, 100);
  text("Choose controlling by duplets ('2') or by triplets ('3')", 40, 150);
  text("The quicker the sequence, the faster the movement", 40, 200);
  text("Check the labels of the semi-axes", 40, 250);  
  text("To start recording, press 'r'", 40, 300);
  text("To follow the ghost, press 'f'", 40, 350);
  text("To get back here, press 'ESC'", 40, 400);
}

void drawGame(){
  background(51);
  switch (sequenceLength) {
    case 2:
      sequenceLength = 2;
      duplets();
      // visualBeatsDuplets(); // uncomment for visual feedback on rhythm
      break;
    case 3:
      sequenceLength = 3;
      triplets();
      // visualBeatsTriplets(); // uncomment for visual feedback on rhythm
      break;
  }
  if (recording) {
    TableRow newRow = table.addRow();
    if (following) {
      fill(121);
      if (frameNum/decimation < (ghostTable.getRowCount() - 1)) 
        {
          int x, y;
          int fN = frameNum/decimation;
          float delta = frameNum%decimation;
          TableRow row = ghostTable.getRow(fN);
          TableRow rown = ghostTable.getRow(fN + 1);
          // x =  int(row.getFloat("x")*scale);  
          // y = int(row.getFloat("y")*scale);  
          /* interpolate ghost positions */
          x = int(((rown.getFloat("x") - row.getFloat("x"))/decimation * delta + row.getFloat("x"))*scale);
          y = int(((rown.getFloat("y") - row.getFloat("y"))/decimation * delta + row.getFloat("y"))*scale); 
          ellipse(x, y, 10, 10);
          frameNum++;
          newRow.setFloat("xGhost", x);
          newRow.setFloat("yGhost", y);
        }
    }
    text("recording", width/2 - 30, height/2); 
    fill(255);
    int which = frameCount % NUM;
    newRow.setInt("frame", frameCount);
    newRow.setFloat("x", mx[which]);
    newRow.setFloat("y", my[which]);
    newRow.setFloat("IOIx", speedH);
    newRow.setFloat("IOIy", speedV);
    newRow.setFloat("frameRate", frameRate);
    newRow.setFloat("time", millis());
  }
}

void draw() {
  if (screenState == MENUSCREEN) {
    drawMenu();
  } else if (screenState == GAMESCREEN) {
    drawGame();
  } else {
    print("Something went wrong!\n");
  }
}

void keyPressed() {
  if (key == ESC) {
    key = 0;
    screenState = MENUSCREEN;
    if (recording) {
      if (following) saveTable(table, "data/path_follow_" + day() + hour() + minute() + ".csv");
      else  saveTable(table, "data/path_" + day() + hour() + minute() + ".csv");
      recording = false;
      following = false;
      frameNum = 0;
    }
    return;
  }
  else if (key != CODED) {
    if (key == '2') sequenceLength=2;
    if (key == '3') sequenceLength=3;
    screenState = GAMESCREEN;
    if (key == 'r') {
      recording = true;
      table = new Table();
      table.addColumn("frame");
      table.addColumn("x");
      table.addColumn("y");
      table.addColumn("IOIx");
      table.addColumn("IOIy");
      table.addColumn("frameRate");
      table.addColumn("time");
      table.addColumn("W = " + str(width));
      table.addColumn("H = " + str(height));
      table.addColumn("taps = " + str(sequenceLength));
    }
    if (key == 'f') {
      recording = true;
      following = true;
      table = new Table();
      table.addColumn("frame");
      table.addColumn("x");
      table.addColumn("y");
      table.addColumn("IOIx");
      table.addColumn("IOIy");
      table.addColumn("xGhost");
      table.addColumn("yGhost");
      table.addColumn("frameRate");
      table.addColumn("time");
      table.addColumn("W = " + str(width));
      table.addColumn("H = " + str(height));
      table.addColumn("taps = " + str(sequenceLength));
      TableRow row = ghostTable.getRow(2); // initialize follower to ghost position
      for (int i=0; i<NUM; i++) {
          mx[i] = int(row.getFloat("x")*scale);
          my[i] = int(row.getFloat("y")*scale);  
      } 
    }
    // if (key == 'q') exit();
    return;
  }
  
  inst = millis();
  if ((inst - inst_1) > 2000) {
     t = 0;  // restart from first pulse after 2 seconds of inactivity
             // non robusto, bisognerebbe usare l'accento
     tap[0]='N';
     tap[1]='N';
     tap[2]='N';                   
  }
  if ((pressed == true) && ((inst - inst_1) < 60)){ // two keys simultaneously pressed
                                                    // threshold = 80ms
     t = t - 1; if (t<0) t = sequenceLength - 1;
     tap[t] = 'X'; 
  }
  else {
    ioi[t] = inst - inst_1;
    if (keyCode == LEFT) {
        tap[t] = 'L';
        tap[t+1] = 'N'; // to avoid false turns after first tap
    }
    if (keyCode == RIGHT) {
        tap[t] = 'R';
        tap[t+1] = 'N'; // to avoid false turns after first tap
    }
  }
  t = (t+1)%sequenceLength;
  inst_1 = inst;
  pressed = true;
}

void keyReleased() {
   pressed = false; 
   for (int i=0; i<sequenceLength; i++) print(tap[i] + " ");
   print("\n");
}

void noteOn(int channel, int pitch, int velocity) {
  // Receive a noteOn
  /* println();
  println("Note On:");
  println("--------");
  println("Channel:"+channel);
  println("Pitch:"+pitch);
  println("Velocity:"+velocity); */
  inst = millis();
  if ((inst - inst_1) > 2000) {
     t = 0;  // restart from first pulse after 2 seconds of inactivity
             // non robusto, bisognerebbe usare l'accento
     tap[0]='N';
     tap[1]='N';
     tap[2]='N';                   
  }
  if ((pressed == true) && ((inst - inst_1) < 60)){ // two keys simultaneously pressed
                                                    // threshold = 80ms
     t = t - 1; if (t<0) t = sequenceLength - 1;
     tap[t] = 'X'; 
  }
  else {
    ioi[t] = inst - inst_1;
    if (pitch == 39) {
        tap[t] = 'L';
        tap[t+1] = 'N'; // to avoid false turns after first tap
        lamp[t] = velocity/127.0;
    }
    if (pitch == 51) {
        tap[t] = 'R';
        tap[t+1] = 'N'; // to avoid false turns after first tap
        ramp[t] = velocity/127.0;
    }
  }
  t = (t+1)%sequenceLength;
  inst_1 = inst;
  pressed = true;
  if (pitch == 39) {tapL.amp(0.01 + lamp[t]/5); tapL.cue(0); tapL.play();}
  if (pitch == 51) {tapR.amp(0.01 + ramp[t]/5); tapR.cue(0); tapR.play();}
}

void noteOff(int channel, int pitch, int velocity) {
  // Receive a noteOff
  /* println();
  println("Note Off:");
  println("--------");
  println("Channel:"+channel);
  println("Pitch:"+pitch);
  println("Velocity:"+velocity); */
  pressed = false; 
  for (int i=0; i<sequenceLength; i++) print(tap[i] + " ");
  print("\n");
}


void cpioi(char dir) {
  for (int i=0; i<sequenceLength; i++) 
    if (dir=='H') ioiH[i] = ioi[i];
    else ioiV[i] = ioi[i];
}
