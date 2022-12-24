void triplets(){
  text("RRR", width - 40, height/2); 
  text("LLL", 10, height/2); 
  text("LRL", width/2 - 2, 30); 
  text("XXX", width/2 - 1, height-17); 

  int which = frameCount % NUM;
  mx[which] = mx[prev] + SPEEDMUL/speedH; // = mouseX; 
  mx[which] = constrain(mx[which], 0, width);
  my[which] = my[prev] + SPEEDMUL/speedV; // = mouseY;
  my[which] = constrain(my[which], 0, height);
  prev = which;

  /*
  RRR = right
  LLL = left
  LRL = up
  XXX = down
  */
  if ((tap[0]=='R') && (tap[1]=='R') && (tap[2]=='R')) {
   cpioi('H');
   speedH = (ioiH[1] + ioiH[2]) / 2;
  }
  if ((tap[0]=='L') && (tap[1]=='L') && (tap[2]=='L')) {
   cpioi('H');
   speedH = -(ioiH[1] + ioiH[2]) / 2;
  }
  if (((tap[0]=='L') && (tap[1]=='R') && (tap[2]=='L')) || ((tap[0]=='R') && (tap[1]=='L') && (tap[2]=='R'))) {
   cpioi('V');
   speedV = -(ioiV[1] + ioiV[2]) / 2;
   if (tap[0] == 'R') { xamp[0] = ramp[0]; uuamp[1] = lamp[1]; uuamp[2] = ramp[2]; }
   else { xamp[0] = lamp[0]; uuamp[1] = ramp[1]; uuamp[2] = lamp[2]; }
  }
  if ((tap[0]=='X') && (tap[1]=='X') && (tap[2]=='X')) {
   cpioi('V');
   speedV = (ioiV[1] + ioiV[2]) / 2;
   xamp[0] = ramp[0];
   xamp[1] = ramp[1];
   xamp[2] = ramp[2];
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
       if (countH == 1) {r.amp(ramp[1]); r.cue(0); r.play();};
       if (countH == 2) {r.amp(ramp[2]); r.cue(0); r.play();}
     }
     else { // leftward
       if (countH == 0) {l.amp(lamp[0]); l.cue(0); l.play();}
       if (countH == 1) {l.amp(lamp[1]); l.cue(0); l.play();}
       if (countH == 2) {l.amp(lamp[2]); l.cue(0); l.play();}
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
       if (countV == 2) {x.amp(xamp[2]); x.cue(0); x.play();}
     }
     else { // upward
       if (countV == 0) {x.amp(xamp[0]); x.cue(0); x.play();}
       if (countV == 1) {u.amp(uuamp[1]); u.cue(0); u.play();}
       if (countV == 2) {uu.amp(uuamp[2]); uu.cue(0); uu.play();}
     }
     countV = (countV+1)%sequenceLength;
     if (countV == 0) {
       triggerV = millis() + abs((int)speedV) + PAUSE; 
     }
     else triggerV = millis() + abs(int(ioiV[countV]));

  }
}
