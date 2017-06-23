package qnactr.objectDesigner;
import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JTextField;

import qnactr.sim.QnactrSimulation;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import java.io.IOException; 
import java.net.DatagramPacket; 
import java.net.DatagramSocket; 
import java.net.InetAddress; 
import java.net.SocketException;
//import SwapData1;


public class UnityJavaUdp {

	public JFrame frame;
	private JTextField IntValue1;
	private JTextField FloatValue1;
	private JTextField DoubleValue1;
	private JTextField BoolValue1;
	private JTextField BoolValue2;
	private JTextField DoubleValue2;
	private JTextField FloatValue2;
	private JTextField IntValue2;
	
	public SwapData sdsend = new SwapData();
	public SwapData sdreceive = new SwapData();
 	private DatagramSocket socket=null;
 	private byte[] buffer=new byte[1024];
 	
 	private int unityReceivePortID = 8000;
 	private int javaReceivePortID = 8001;
 	QnactrSimulation sim; //pointer
	
	/**
	 * Launch the application.
	 */
//	public static void main(String[] args) {
//		EventQueue.invokeLater(new Runnable() {
//			public void run() {
//				try {
//					UnityJavaUdp window = new UnityJavaUdp();
//					window.frame.setVisible(true);
//				} catch (Exception e) {
//					e.printStackTrace();
//				}
//			}
//		});
//	}

	/**
	 * Create the application.
	 */
	
	
	
	public UnityJavaUdp() {
		initialize();
		
		try {
			socket=new DatagramSocket(javaReceivePortID);
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
//		 new Thread() {
//             public void run() {
//             	Receive();
//             }
//         }.start();

	}

	public UnityJavaUdp(QnactrSimulation simPointer) {
		
		sim = simPointer;
				
		initialize();
		
		try {
			socket=new DatagramSocket(javaReceivePortID);
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
//		 new Thread() {
//             public void run() {
//             	Receive();
//             }
//         }.start();

	}
	
	
	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		
		switch (sim.vars.programGlobalVar__Use_Predefined_Model_Setup){
			case "unity_tangtang_2015":
		    {
				//for unity_tangtang_2015
				sdsend.a1 = 1;  // this is for decision result, i.e., status 1 = search; 2 = avoid; 3 = shoot; 4 = dead.
				//b1 c1	 d1 values are not needed in the data output to unity.
				
				sdreceive.b1 = 0.0f; // this stores the distance between self and the closest target
				sdreceive.c1 = 0.0;  // this stores the unity engine simulation clock time.
				
		    	break;
		    }
		}

		
		frame = new JFrame();
		frame.setBounds(100, 100, 489, 424);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		
		JLabel label = new JLabel("receive");
		label.setBounds(85, 33, 54, 15);
		frame.getContentPane().add(label);
		
		JButton button = new JButton("manually send");
		button.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent arg0) {
				try {
					buffer=new byte[1024];
					sdsend=new SwapData(Integer.parseInt(IntValue2.getText().trim()),Float.parseFloat(FloatValue2.getText().trim()),Double.parseDouble(DoubleValue2.getText().trim()),Boolean.parseBoolean(BoolValue2.getText().trim()));
				    buffer = sdsend.getBytes();
				    DatagramPacket packet = new DatagramPacket(buffer, buffer.length, InetAddress.getByName("127.0.0.1"), unityReceivePortID);				
				
					socket.send(packet);
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				} //Thread.sleep(2000); 
			}
		});
		button.setBounds(322, 29, 93, 23);
		frame.getContentPane().add(button);
		
		JLabel lblIntvalue = new JLabel("IntValue");
		lblIntvalue.setBounds(191, 81, 54, 15);
		frame.getContentPane().add(lblIntvalue);
		
		JLabel lblFloatvalue = new JLabel("FloatValue");
		lblFloatvalue.setBounds(191, 138, 72, 15);
		frame.getContentPane().add(lblFloatvalue);
		
		JLabel lblDoublevalue = new JLabel("DoubleValue");
		lblDoublevalue.setBounds(191, 192, 72, 15);
		frame.getContentPane().add(lblDoublevalue);
		
		JLabel lblBoolvalue = new JLabel("BoolValue");
		lblBoolvalue.setBounds(191, 241, 72, 15);
		frame.getContentPane().add(lblBoolvalue);
		
		IntValue1 = new JTextField();
		IntValue1.setBounds(38, 81, 101, 23);
		frame.getContentPane().add(IntValue1);
		IntValue1.setColumns(10);
		
		FloatValue1 = new JTextField();
		FloatValue1.setColumns(10);
		FloatValue1.setBounds(38, 138, 101, 23);
		frame.getContentPane().add(FloatValue1);
		
		DoubleValue1 = new JTextField();
		DoubleValue1.setColumns(10);
		DoubleValue1.setBounds(38, 192, 101, 23);
		frame.getContentPane().add(DoubleValue1);
		
		BoolValue1 = new JTextField();
		BoolValue1.setColumns(10);
		BoolValue1.setBounds(38, 241, 101, 23);
		frame.getContentPane().add(BoolValue1);
		
		BoolValue2 = new JTextField();
		BoolValue2.setColumns(10);
		BoolValue2.setBounds(314, 241, 101, 23);
		frame.getContentPane().add(BoolValue2);
		
		DoubleValue2 = new JTextField();
		DoubleValue2.setColumns(10);
		DoubleValue2.setBounds(314, 192, 101, 23);
		frame.getContentPane().add(DoubleValue2);
		
		FloatValue2 = new JTextField();
		FloatValue2.setColumns(10);
		FloatValue2.setBounds(314, 138, 101, 23);
		frame.getContentPane().add(FloatValue2);
		
		IntValue2 = new JTextField();
		IntValue2.setColumns(10);
		IntValue2.setBounds(314, 77, 101, 23);
		frame.getContentPane().add(IntValue2);
		
		
	}
	
	public void Receive(){
		//System.out.println("Thread Start");

		byte buffer1[] = new byte[1024]; 
		try {


			buffer1=new byte[buffer1.length];
			
			DatagramPacket packet = new DatagramPacket(buffer1, buffer1.length); 			
				socket.receive(packet);
				Thread.sleep(1);
				sdreceive.getSwapData(new String(packet.getData()));
				IntValue1.setText(String.valueOf(sdreceive.a1));
				FloatValue1.setText(String.valueOf(sdreceive.b1));
				DoubleValue1.setText(String.valueOf(sdreceive.c1));
				BoolValue1.setText(String.valueOf(sdreceive.d1));
				

				System.out.println(sdreceive.a1);//鍞愮�?//
				System.out.println(sdreceive.b1);//Cao//
				System.out.println(sdreceive.c1);//Cao//
				System.out.println(sdreceive.d1);//Cao//
				System.out.println(" ");//Cao//
				

		} catch (IOException |InterruptedException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}
	
	public void Send(){
		try {
			buffer = new byte[1024];			
		    buffer = sdsend.getBytes();
		    DatagramPacket packet2 = new DatagramPacket(buffer, buffer.length, InetAddress.getByName("127.0.0.1"), unityReceivePortID);				// "127.0.0.1" is for self computer
		
			socket.send(packet2);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}
	
	

	public class SwapData
	{
	    public SwapData() { 
	    	a1 = 0;
	        b1 = 0;
	        c1 = 0;
	        d1 = false;
	    }
	    public SwapData(int a, float b, double c, boolean d)
	    {
	        a1 = a;
	        b1 = b;
	        c1 = c;
	        d1 = d;
	    }
	    public byte[] getBytes()
	    {
	        String str = String.valueOf(a1) + "," +String.valueOf(b1) + "," +String.valueOf(c1) + "," + String.valueOf(d1);
	        byte[] bytes = str.getBytes();
	        return bytes;
	    }
	    public void getSwapData(String message)
	    {
	    	//System.out.println(4);
	        String[] str = message.split(",");
	       // System.out.println(2);
	        //string[] str1 = str.Split(new char[]{','});
	        //return new SwapData1(Integer.parseInt(str[0].trim()), Float.parseFloat(str[1].trim()), Double.parseDouble(str[2].trim()), Boolean.parseBoolean(str[3].trim()));
	        if(str.length==4){
	        	a1=Integer.parseInt(str[0].trim());
	            //System.out.println(3);
	            b1=Float.parseFloat(str[1].trim());
	            c1=Double.parseDouble(str[2].trim());
	            d1=Boolean.parseBoolean(str[3].trim());
	        }else{
	        	a1 = 0;
	            b1 = 0;
	            c1 = 0;
	            d1 = false;
	        }        
	    }
	    public int a1;
	    public float b1;
	    public double c1;
	    public boolean d1;
	}
}
