package qnactr.sim;

import java.nio.ByteBuffer;

import javax.sound.sampled.*;

public class SoundUtilities
{
  
  public static float SAMPLE_RATE = 8000f;
  
  public static void tone(int hz, int msecs) 
      throws LineUnavailableException 
      {
    tone(hz, msecs, 1.0);
      }
  
  public static void tone(int hz, int msecs, double vol)
      throws LineUnavailableException 
      {
    byte[] buf = new byte[1];
    AudioFormat af = 
        new AudioFormat(
            SAMPLE_RATE, // sampleRate
            8,           // sampleSizeInBits
            1,           // channels
            true,        // signed
            false);      // bigEndian
    SourceDataLine sdl = AudioSystem.getSourceDataLine(af);
    
    
    sdl.open(af);
    sdl.start();
    for (int i=0; i < msecs*8; i++) {
      double angle = i / (SAMPLE_RATE / hz) * 2.0 * Math.PI;
      buf[0] = (byte)(Math.sin(angle) * 127.0 * vol);
      sdl.write(buf,0,1);
    }
    sdl.drain();
    sdl.stop();
    sdl.close();
      }
  
  
  public static void testToneSound (double fFreq ) throws InterruptedException, LineUnavailableException {
    
    final int SAMPLING_RATE = 44100;            // Audio sampling rate
    final int SAMPLE_SIZE = 2;                  // Audio sample size in bytes

    SourceDataLine line;
    
    //double fFreq = 440;                         // Frequency of sine wave in hz

    //Position through the sine wave as a percentage (i.e. 0 to 1 is 0 to 2*PI)
    double fCyclePosition = 0;        

    //Open up audio output, using 44100hz sampling rate, 16 bit samples, mono, and big 
    // endian byte ordering
    AudioFormat format = new AudioFormat(SAMPLING_RATE, 16, 1, true, true);
    DataLine.Info info = new DataLine.Info(SourceDataLine.class, format);

    if (!AudioSystem.isLineSupported(info)){
       System.out.println("Line matching " + info + " is not supported.");
       throw new LineUnavailableException();
    }

    line = (SourceDataLine)AudioSystem.getLine(info);
    line.open(format);  
    line.start();

    // Make our buffer size match audio system's buffer
    ByteBuffer cBuf = ByteBuffer.allocate(line.getBufferSize());   

    int ctSamplesTotal = SAMPLING_RATE*5;         // Output for roughly 5 seconds


    //On each pass main loop fills the available free space in the audio buffer
    //Main loop creates audio samples for sine wave, runs until we tell the thread to exit
    //Each sample is spaced 1/SAMPLING_RATE apart in time
    while (ctSamplesTotal>0) {
       double fCycleInc = fFreq/SAMPLING_RATE;  // Fraction of cycle between samples

       cBuf.clear();                            // Discard samples from previous pass

        // Figure out how many samples we can add
       int ctSamplesThisPass = line.available()/SAMPLE_SIZE;   
       for (int i=0; i < ctSamplesThisPass; i++) {
          cBuf.putShort((short)(Short.MAX_VALUE * Math.sin(2*Math.PI * fCyclePosition)));

          fCyclePosition += fCycleInc;
          if (fCyclePosition > 1)
             fCyclePosition -= 1;
       }

       //Write sine samples to the line buffer.  If the audio buffer is full, this will 
       // block until there is room (we never write more samples than buffer will hold)
       line.write(cBuf.array(), 0, cBuf.position());            
       ctSamplesTotal -= ctSamplesThisPass;     // Update total number of samples written 

       //Wait until the buffer is at least half empty  before we add more
       while (line.getBufferSize()/2 < line.available())   
          Thread.sleep(1);                                             
    }


    //Done playing the whole waveform, now wait until the queued samples finish 
    //playing, then clean up and exit
    line.drain();                                         
    line.close();
    
  }
  
  
  
  
  public static void main(String[] args) throws Exception{
    
    SoundUtilities.tone(1000,1000);
    //Thread.sleep(1000);
    SoundUtilities.tone(5000,1000);

    Thread.sleep(1000);
    SoundUtilities.tone(400,1000);
    Thread.sleep(1000);
    SoundUtilities.tone(400,1000, 0.2);
    
//    try
//    {
//      SoundFactory.testToneSound(440);
//      
//      SoundFactory.testToneSound(880);
//    } catch (InterruptedException e)
//    {
//      // TODO Auto-generated catch block
//      e.printStackTrace();
//    } catch (LineUnavailableException e)
//    {
//      // TODO Auto-generated catch block
//      e.printStackTrace();
//    }  

    
  }
}
