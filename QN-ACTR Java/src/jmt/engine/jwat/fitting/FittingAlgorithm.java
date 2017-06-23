package jmt.engine.jwat.fitting;

public interface FittingAlgorithm {
	public boolean isFitted();
	public double[] generateQQPlot();
	public boolean isLastRunFitted();
	public double[] getEstimatedParameters();
}
