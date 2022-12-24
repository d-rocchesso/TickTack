void duplets(){
  text("RR", width - 40, height/2); 
  text("LL", 10, height/2); 
  text("LR", width/2 - 2, 30); 
  text("XX", width/2 - 1, height-17); 

  int which = frameCount % NUM;
  mx[which] = mx[prev] + SPEEDMUL/speedH; // = mouseX; 
  mx[which] = constrain(mx[which], 0, width);
  my[which] = my[prev] + SPEEDMUL/speedV; // = mouseY;
  my[which] = constrain(my[which], 0, height);
  prev = which;

  /*
  RR = right
  LL = left
  LR = up
  XX = down
  */
  if ((tap[0]=='R') && (tap[1]=='R')) {
   cpioi('H');
   speedH = ioiH[1];
  }
  if ((tap[0]=='L') && (tap[1]=='L')) {
   cpioi('H');
   speedH = -ioiH[1];
  }
  if (((tap[0]=='L') && (tap[1]=='R')) || ((tap[0]=='R') && (tap[1]=='L'))) {
   cpioi('V');
   speedV = -ioiV[1];
   if (tap[0] == 'R') { xamp[0] = ramp[0]; uuamp[1] = lamp[1]; }
   else { xamp[0] = lamp[0]; uuamp[1] = ramp[1]; }
  }
  if ((tap[0]=='X') && (tap[1]=='X')) {
   cpioi('V');
   speedV = ioiV[1];
   xamp[0] = ramp[0];
   xamp[1] = ramp[1]; 
  }

  noFill();
  stroke(200);
  if (speedH > 0) 
   ellipse(width - 25, height/2-5, 44, 44);
  else
   ellipse(25, height/2-5, 44, 44);
  if (speedV > 0) 
   ellipse(width/2 + 13, height - 25, 44, 44);
  else
   ellipse(width/2 + 13, 25, 44, 44);
  noStroke();
  fill(255);

  for (int i = 0; i < NUM; i++) {
    int index = (which+1 + i) % NUM;
    ellipse(mx[index], my[index], float(i)/NUM*10, float(i)/NUM*10);
  }
  
  if (triggerH < millis()) {
     if (speedH >= 0) { // rightward
       if (countH == 0) {r.amp(ramp[0]); r.cue(0); r.play();}
       if (countH == 1) {r.amp(ramp[1]); r.cue(0); r.play();}
     }
     else { // leftward
       if (countH == 0) {l.amp(lamp[0]); l.cue(0); l.play();}
       if (countH == 1) {l.amp(lamp[1]); l.cue(0); l.play();}
     }
     countH = (countH+1)%sequenceLength;   
     if (countH == 0) {
       triggerH = millis() + abs((int)speedH) + PAUSE; 
     }
     else triggerH = millis() + abs(int(ioiH[countH]));
  }
  
  if (triggerV < millis()) {
     if (speedV >= 0) { // downward
       if (countV == 0) {x.amp(xamp[0]); x.cue(0); x.play();}
       if (countV == 1) {x.amp(xamp[1]); x.cue(0); x.play();}
     }
     else { // upward
       if (countV == 0) {x.amp(xamp[0]); x.cue(0); x.play();}
       if (countV == 1) {uu.amp(uuamp[0]); uu.cue(0); uu.play();}
     }
     countV = (countV+1)%sequenceLength;
     if (countV == 0) {
       triggerV = millis() + abs((int)speedV) + PAUSE; 
     }
     else triggerV = millis() + abs(int(ioiV[countV]));

  }
}
