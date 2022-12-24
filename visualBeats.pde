void visualBeatsDuplets() {
  fill(color(255,0,0));
  // Horizontal
  if (triggerBeatH < millis()) {    
    fc1 = frameCount + PERSISTENCE;
    triggerBeatH = millis() + int(2*ioiH[1]) + PAUSE;    
    triggerBeatH1 = millis() + int(ioiH[1]);
  }
  if (triggerBeatH1 < millis()) { 
    fc2 = frameCount + PERSISTENCE;
    triggerBeatH1 = millis() + int(2*ioiH[1]) + PAUSE;  
  }
  if (frameCount < fc1) {
    ellipse(width - 10, height/2, 10, 10);
    ellipse(10, height/2, 10, 10);
  }
  if (frameCount < fc2) {
    ellipse(width - 10, height/2, 10, 10);
    ellipse(10, height/2, 10, 10);
  }
  //  Vertical 
  if (triggerBeatV < millis()) {    
    fcv1 = frameCount + PERSISTENCE;
    triggerBeatV = millis() + int(2*ioiV[1]) + PAUSE;    
    triggerBeatV1 = millis() + int(ioiV[1]);
  }
  if (triggerBeatV1 < millis()) { 
    fcv2 = frameCount + PERSISTENCE;
    triggerBeatV1 = millis() + int(2*ioiV[1]) + PAUSE;   
  }
  
  if (frameCount < fcv1) {
    ellipse(width/2, 30, 10, 10);
    ellipse(width/2 , height - 17, 10, 10);
    ellipse(width/2 + 30, height - 17, 10, 10);
  }
  if (frameCount < fcv2) {
    ellipse(width/2 + 30, 10, 10, 10);
    ellipse(width/2, height - 17, 10, 10);
    ellipse(width/2 + 30, height - 17, 10, 10);
  } 
  fill(color(255,255,255));
}

void visualBeatsTriplets() {
  fill(color(255,0,0));
  // Horizontal
  if (triggerBeatH < millis()) {    
    fc1 = frameCount + PERSISTENCE;
    triggerBeatH = millis() + int((ioiH[1] + ioiH[2])/2 + ioiH[1] + ioiH[2]) + PAUSE;    
    triggerBeatH1 = millis() + int(ioiH[1]);
    triggerBeatH2 = millis() + int(ioiH[1] + ioiH[2]);
  }
  if (triggerBeatH1 < millis()) { 
    fc2 = frameCount + PERSISTENCE;
    triggerBeatH1 = millis() + int((ioiH[1] + ioiH[2])/2 + ioiH[1] + ioiH[2]) + PAUSE;  
  }
  if (triggerBeatH2 < millis()) {     
    fc3 = frameCount + PERSISTENCE;
    triggerBeatH2 = millis() + int((ioiH[1] + ioiH[2])/2 + ioiH[1] + ioiH[2]) + PAUSE;   
  }
  if (frameCount < fc1) {
    ellipse(width - 10, height/2, 10, 10);
    ellipse(10, height/2, 10, 10);
  }
  if (frameCount < fc2) {
    ellipse(width - 10, height/2, 10, 10);
    ellipse(10, height/2, 10, 10);
  }
  if (frameCount < fc3) { 
    ellipse(width - 10, height/2, 10, 10);
    ellipse(10, height/2, 10, 10);
  }
  //  Vertical 
  if (triggerBeatV < millis()) {    
    fcv1 = frameCount + PERSISTENCE;
    triggerBeatV = millis() + int((ioiV[1] + ioiV[2])/2 + ioiV[1] + ioiV[2]) + PAUSE;    
    triggerBeatV1 = millis() + int(ioiV[1]);
    triggerBeatV2 = millis() + int(ioiV[1] + ioiV[2]);
  }
  if (triggerBeatV1 < millis()) { 
    fcv2 = frameCount + PERSISTENCE;
    triggerBeatV1 = millis() + int((ioiV[1] + ioiV[2])/2 + ioiV[1] + ioiV[2]) + PAUSE;   
  }
  if (triggerBeatV2 < millis()) {     
    fcv3 = frameCount + PERSISTENCE;
    triggerBeatV2 = millis() + int((ioiV[1] + ioiV[2])/2 + ioiV[1] + ioiV[2]) + PAUSE;    
  }
  
  if (frameCount < fcv1) {
    ellipse(width/2, 30, 10, 10);
    ellipse(width/2 , height - 17, 10, 10);
    ellipse(width/2 + 30, height - 17, 10, 10);
  }
  if (frameCount < fcv2) {
    ellipse(width/2 + 30, 20, 10, 10);
    ellipse(width/2, height - 17, 10, 10);
    ellipse(width/2 + 30, height - 17, 10, 10);
  }
  if (frameCount < fcv3) { 
    ellipse(width/2, 15, 10, 10);
    ellipse(width/2, height - 17, 10, 10);
    ellipse(width/2 + 30, height - 17, 10, 10);
  }
  
  fill(color(255,255,255));
}
