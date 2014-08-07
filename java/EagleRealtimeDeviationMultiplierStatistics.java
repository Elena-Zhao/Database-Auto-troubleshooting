package com.ebay.eagle.slidewindow;

import org.apache.commons.math.stat.descriptive.DescriptiveStatistics;

/**
 * not thread safe, all addValue, addValueAndRemove invocation must be serialized from outside
 * mad - median absolute deviation
 * dm - deviation multiplier
 * A window to keep recent windowSize data points then calculate mad and deviation multiplier
 * @author yonzhang
 *
 */
public class EagleRealtimeDeviationMultiplierStatistics {
	private double madBaseLinePercentile = 90.0;
	private int windowSize;
	private final static double MEDIAN_PERCENTILE = 50.0;
	
	private double deviationMultiplier;
	private long numElements = 0;
	
	public EagleRealtimeDeviationMultiplierStatistics(int windowSize){
		this(windowSize, 90.0);
	}
	public EagleRealtimeDeviationMultiplierStatistics(int windowSize, double madPercentile){
		if(windowSize <=0)
			throw new IllegalArgumentException("window size must be larger than 0, now it is " + windowSize);
		this.windowSize = windowSize;
		stats = new DescriptiveStatistics(windowSize);
		this.madBaseLinePercentile = madPercentile;
	}
	
	/**
	 * @param value
	 * @return true when this value exceed mad baseline, false otherwise or when the window size is not reached
	 */
	public boolean addValue(double value){
		stats.addValue(value);
		if(++numElements < windowSize){
			return false;
		}
		double madbase = stats.getPercentile(madBaseLinePercentile);
		// calculate mad
		DescriptiveStatistics madStats = new DescriptiveStatistics(windowSize);
		for(int i=0; i<windowSize; i++){
			double deviation = Math.abs(stats.getElement(i) - madbase);
			madStats.addValue(deviation);
		}
		double mad = madStats.getPercentile(MEDIAN_PERCENTILE);
		double dm = Math.abs(value - madbase) / mad;
		deviationMultiplier = dm;
		return value-madbase > 0 ? true : false;
	}
	
	/**
	 * sometimes we need add value and then delete it immediately because this value is used for testing alert instead of a final value
	 * this method just delegated to addValue and removeMostRecentValue of DescriptiveStatistics
	 * @param value
	 * @return
	 */
	public void removeMostRecentValue(){
		stats.removeMostRecentValue();
	}
	
	public double dm(){
		return this.deviationMultiplier;
	}
	
	public int windowSize(){
		return windowSize;
	}
	
	public long getNumElements() {
		return stats.getN();
	}
}
