package jmt.engine.jwat.fitting;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class ParetoFitting implements FittingAlgorithm {
	private double sigmaML;
	private double alphaML;
	private double[] data;
	private double[] trasf_data;
	private double sign;
	private boolean isPareto;
	
	/**
	 * Constructor. Note that that MUST BE ordered in ascending order by the caller!
	 * @param data
	 * @param sign
	 */
	public ParetoFitting(double[] data,double sign) {
		this.data = data;
		this.sign = sign;
		this.isPareto = false;
		
		calculateParameters();
	}
	
	public double[] getEstimatedParameters() {
		double[] result;
		
		result = new double[2];
		result[0] = sigmaML;
		result[1] = alphaML;
		
		return result;
	}
	
	private void calculateParameters() {
		double summation;
		int n = data.length;
		int i;
		 
		sigmaML = data[0];
		trasf_data = (double[]) data.clone();
		summation = 0.0d;
		for(i=0;i<n;i++) {
			trasf_data[i] /= sigmaML;
			trasf_data[i] = Math.log(trasf_data[i]);
			summation += trasf_data[i];
		}

		alphaML = (n - 2) / summation;
		
		//System.out.println("n is: " + n);
		//System.out.println("------->Stimatore di k: " + sigmaML);
		//System.out.println("------->Stimatore di alpha " + alphaML);
	}
	
	/**
	 * Method for applying the Gulati-Shapiro test of hypothesis. Supposes that the input data is already ordered.
	 * @param data
	 * @param sign
	 * @param est_par
	 * @return
	 */
	public boolean isFitted() {

		double y,ty,usegn,iui;
		double z0,z1,z2;
		int n = data.length;
		int i;
		
		ty = 0.0d;
		usegn = 0.0d;
		iui = 0.0d;
		
		for(i = 1;i <= n-1;i++) {
			
			if(i==1)
				y = (n - i + 1) * trasf_data[i-1];
			else
				y = (n - i + 1) * (trasf_data[i-1] - trasf_data[i-2]);
			
			ty += y;

			usegn += ty;
			iui += i*ty;
		}
		
		usegn /= (n-1)*ty;
		iui /= (n-1)*ty;
		
		//System.out.println("usegn "  + usegn);
		//System.out.println("iui " + iui);
		
		
		z1 = Math.sqrt(12d*(n-1)) * (usegn - 0.5d);
		//System.out.println("z1 "+z1);
		
		/*System.out.println("Z2 = Math.sqrt((5*("+n+"-1))/("+n+"+2)*("+n+"-2))* ("+n+" - 2 + 6*"+n+"*"+usegn+"- 12*"+iui+")");
		System.out.println("QUI "+Math.sqrt((5d*((long)n-1))/(((long)n+2)*((long)n-2))));*/
		
		z2 = Math.sqrt((5d*((long)n-1))/(((long)n+2)*((long)n-2))) * ((long) n - 2 + 6*n*usegn - 12*iui);
		//System.out.println("z2 "+z2);
	
		z0 = Math.pow(z1, 2) + Math.pow(z2, 2);
		//System.out.println("z0 "+z0);
		
		//System.out.println("p-value: " + Math.pow(Math.E, -z0/2));
		
		this.isPareto = z0 <= -2*Math.log(sign);
		
		return this.isPareto;
	}
	
	public boolean isLastRunFitted() {
		return this.isPareto;
	}
	
	public double[] generateQQPlot() {
		double[] y;
		
		//PlotGraph pg;
		
		int n = data.length;
		int i;
		
		y = new double[n];
		
		for(i=0;i<n;i++) {
			y[i] = sigmaML/Math.pow(1d-((double) i+1)/((double) n+1),1d/alphaML);
			
			//System.out.println("Original value: " + data[i] + "  Inverted value: " + y[i]);
		}
		
		return y;
		
		//pg = new  PlotGraph(trasf_data, y);
		//pg.plot();
		
	}
}
