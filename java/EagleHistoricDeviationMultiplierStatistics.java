package com.ebay.eagle.slidewindow;

import org.apache.commons.math.stat.descriptive.DescriptiveStatistics;

import com.ebay.eagle.metric.MetricContext;
import com.ebay.eagle.metric.MetricListener;

/**
 * Unbounded statistics to calculate a certain percentile deviation multiplier 
 * Not thread-safe
 * mad - median absolute deviation
 * dm - deviation multiplier
 * @author yonzhang
 *
 */
public class EagleHistoricDeviationMultiplierStatistics implements MetricListener{
	private EagleRealtimeDeviationMultiplierStatistics realTimeDMImpl; 
	private final static double dmPercentile = 95.0;
	
	// set max data point to avoid memory climbing up continuously
	private int maxDataPoints = 10000;
	private final static int defaultWindowSize = 100;
	
	private DescriptiveStatistics dmStats;

	public EagleHistoricDeviationMultiplierStatistics(){
		this(defaultWindowSize);
	}
	
	public EagleHistoricDeviationMultiplierStatistics(int windowSize){
		this(windowSize, 90.0);
	}
	
	public EagleHistoricDeviationMultiplierStatistics(int windowSize, int maxDataPoints, double madPercentile){
		this.maxDataPoints = maxDataPoints;
		realTimeDMImpl = new EagleRealtimeDeviationMultiplierStatistics(windowSize, madPercentile);
		this.dmStats = new DescriptiveStatistics(maxDataPoints);
	}
	
	public EagleHistoricDeviationMultiplierStatistics(int windowSize, double madPercentile){
		realTimeDMImpl = new EagleRealtimeDeviationMultiplierStatistics(windowSize, madPercentile);
		this.dmStats = new DescriptiveStatistics(maxDataPoints);
	}
	
	public void addValue(double value){
		realTimeDMImpl.addValue(value);
		double dm = realTimeDMImpl.dm();
		dmStats.addValue(dm);
	}
	
	public double getDeviationMultiplier(){
		return getDeviationMultiplier(dmPercentile);
	}
	
	public double getDeviationMultiplier(double percentile){
		return dmStats.getPercentile(percentile);
	}

	public long getNumElements() {
		return realTimeDMImpl.getNumElements();
	}
	
	@Override
	public void onMetric(long timestamp, double v, MetricContext context){
		addValue(v);
	}
}
