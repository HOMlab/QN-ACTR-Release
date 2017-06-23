package jmt.engine.QueueNet;

import java.util.List;

import jmt.common.exception.NetException;
import jmt.engine.dataAnalysis.InverseMeasure;
import jmt.engine.dataAnalysis.Measure;

/**
 * <p><b>Name:</b> PSJobInfoList</p> 
 * <p><b>Description:</b> 
 * A wrapper around JobInfoList to be used by processor sharing. All the estimates are manually set, with the exception
 * of Throughput and Drop rates that are handled by the inner list (as they are unaffected by the Processor Sharing 
 * behavior)
 * </p>
 * <p><b>Date:</b> 10/nov/2009
 * <b>Time:</b> 08.33.59</p>
 * @author Bertoli Marco [marco.bertoli@neptuny.com]
 * @version 3.0
 */
public class PSJobInfoList implements JobInfoList {
	/** Wrapped JobInfoList */
	private JobInfoList list;

	private int numberOfJobClasses;
	private int size, sizePerClass[];

	//arrivals and completions
	private int jobsIn, jobsOut, jobsInPerClass[], jobsOutPerClass[];

	private double busyTime, busyTimePerClass[];

	private double lastJobOutTime, lastJobInTime, lastJobOutTimePerClass[], lastJobInTimePerClass[];

	private Measure utilization, utilizationPerClass[], responseTime, responseTimePerClass[], residenceTime, residenceTimePerClass[], queueLength,
			queueLengthPerClass[];

	private InverseMeasure throughput, throughputPerClass[];

	/** The number of servers to estimate Utilization measure on multiserver environments. */
	private int serverNumber = 1;

	public PSJobInfoList(int numberOfJobClasses, boolean save) {
		this.numberOfJobClasses = numberOfJobClasses;
		list = new LinkedJobInfoList(numberOfJobClasses, save);

		// Initialize all the arrays
		sizePerClass = new int[numberOfJobClasses];
		jobsInPerClass = new int[numberOfJobClasses];
		jobsOutPerClass = new int[numberOfJobClasses];
		busyTimePerClass = new double[numberOfJobClasses];
		lastJobInTimePerClass = new double[numberOfJobClasses];
		lastJobOutTimePerClass = new double[numberOfJobClasses];
	}

	public boolean add(JobInfo jobInfo) {
		return list.add(jobInfo);
	}

	public boolean add(int index, JobInfo jobInfo, boolean isClassTail) {
		return list.add(index, jobInfo, isClassTail);
	}

	public boolean addFirst(JobInfo jobInfo) {
		return list.addFirst(jobInfo);
	}

	public boolean addFirst(JobInfo jobInfo, boolean isClassTail) {
		return list.addFirst(jobInfo, isClassTail);
	}

	public boolean addLast(JobInfo jobInfo, boolean isClassTail) {
		return list.addLast(jobInfo, isClassTail);
	}

	public boolean addLast(JobInfo jobInfo) throws NetException {
		return list.addLast(jobInfo);
	}

	/**--------------------------------------------------------
	 *---------------- "ANALYZE" METHODS ----------------------
	 *--------------------------------------------------------*/

	/* (non-Javadoc)
	 * @see jmt.engine.QueueNet.JobInfoList#analyzeUtilization(jmt.engine.QueueNet.JobClass, jmt.engine.dataAnalysis.Measure)
	 */
	public void analyzeUtilization(JobClass jobClass, Measure Measurement) {
		if (jobClass != null) {
			if (utilizationPerClass == null) {
				utilizationPerClass = new Measure[numberOfJobClasses];
			}
			utilizationPerClass[jobClass.getId()] = Measurement;
		} else {
			utilization = Measurement;
		}
	}

	/* (non-Javadoc)
	 * @see jmt.engine.QueueNet.JobInfoList#analyzeResponseTime(jmt.engine.QueueNet.JobClass, jmt.engine.dataAnalysis.Measure)
	 */
	public void analyzeResponseTime(JobClass jobClass, Measure Measurement) {
		if (jobClass != null) {
			if (responseTimePerClass == null) {
				responseTimePerClass = new Measure[numberOfJobClasses];
			}
			responseTimePerClass[jobClass.getId()] = Measurement;
		} else {
			responseTime = Measurement;
		}
	}

	/* (non-Javadoc)
	 * @see jmt.engine.QueueNet.JobInfoList#analyzeDropRate(jmt.engine.QueueNet.JobClass, jmt.engine.dataAnalysis.InverseMeasure)
	 */
	public void analyzeDropRate(JobClass jobClass, InverseMeasure Measurement) {
		list.analyzeDropRate(jobClass, Measurement);
	}

	/* (non-Javadoc)
	 * @see jmt.engine.QueueNet.JobInfoList#analyzeThroughput(jmt.engine.QueueNet.JobClass, jmt.engine.dataAnalysis.InverseMeasure)
	 */
	public void analyzeThroughput(JobClass JobClass, InverseMeasure Measurement) {
		if (JobClass != null) {
			if (throughputPerClass == null) {
				throughputPerClass = new InverseMeasure[numberOfJobClasses];
			}
			throughputPerClass[JobClass.getId()] = Measurement;
		} else {
			throughput = Measurement;
		}
	}

	/* (non-Javadoc)
	 * @see jmt.engine.QueueNet.JobInfoList#analyzeResidenceTime(jmt.engine.QueueNet.JobClass, jmt.engine.dataAnalysis.Measure)
	 */
	public void analyzeResidenceTime(JobClass JobClass, Measure measurement) {
		if (JobClass != null) {
			if (residenceTimePerClass == null) {
				residenceTimePerClass = new Measure[numberOfJobClasses];
			}
			residenceTimePerClass[JobClass.getId()] = measurement;
		} else {
			residenceTime = measurement;
		}
	}

	/* (non-Javadoc)
	 * @see jmt.engine.QueueNet.JobInfoList#analyzeQueueLength(jmt.engine.QueueNet.JobClass, jmt.engine.dataAnalysis.Measure)
	 */
	public void analyzeQueueLength(JobClass JobClass, Measure measurement) {
		if (JobClass != null) {
			if (queueLengthPerClass == null) {
				queueLengthPerClass = new Measure[numberOfJobClasses];
			}
			queueLengthPerClass[JobClass.getId()] = measurement;
		} else {
			queueLength = measurement;
		}
	}

	public boolean dropJob(Job job) throws NetException {
		return list.dropJob(job);
	}

	public double getBusyTime() throws NetException {
		return busyTime;
	}

	public double getBusyTimePerClass(JobClass jobClass) throws NetException {
		return busyTimePerClass[jobClass.getId()];
	}

	public List<JobInfo> getJobList() {
		return list.getJobList();
	}

	public int getJobsIn() {
		return jobsIn;
	}

	public int getJobsInPerClass(JobClass jobClass) {
		return jobsInPerClass[jobClass.getId()];
	}

	public int getJobsOut() {
		return jobsOut;
	}

	public int getJobsOutPerClass(JobClass jobClass) {
		return jobsOutPerClass[jobClass.getId()];
	}

	public double getLastJobDropTime() {
		return list.getLastJobDropTime();
	}

	public double getLastJobDropTimePerClass(JobClass jobClass) {
		return list.getLastJobDropTimePerClass(jobClass);
	}

	public double getLastJobInTime() {
		return lastJobInTime;
	}

	public double getLastJobInTimePerClass(JobClass jobClass) {
		return lastJobInTimePerClass[jobClass.getId()];
	}

	public double getLastJobOutTime() {
		return lastJobOutTime;
	}

	public double getLastJobOutTimePerClass(JobClass jobClass) {
		return lastJobOutTimePerClass[jobClass.getId()];
	}

	public double getLastModifyTime() {
		if (lastJobOutTime >= lastJobInTime && lastJobOutTime >= list.getLastJobDropTime()) {
			return lastJobOutTime;
		} else if (lastJobInTime >= lastJobOutTime && lastJobInTime >= list.getLastJobDropTime()) {
			return lastJobInTime;
		} else {
			return list.getLastJobDropTime();
		}
	}

	public double getLastModifyTimePerClass(JobClass jobClass) {
		if (lastJobOutTimePerClass[jobClass.getId()] >= lastJobInTimePerClass[jobClass.getId()]
				&& lastJobOutTimePerClass[jobClass.getId()] >= list.getLastJobDropTimePerClass(jobClass)) {
			return lastJobOutTimePerClass[jobClass.getId()];
		} else if (lastJobInTimePerClass[jobClass.getId()] >= lastJobOutTimePerClass[jobClass.getId()]
				&& lastJobInTimePerClass[jobClass.getId()] >= list.getLastJobDropTimePerClass(jobClass)) {
			return lastJobInTimePerClass[jobClass.getId()];
		} else {
			return list.getLastJobDropTimePerClass(jobClass);
		}
	}

	public JobInfo lookFor(Job job) throws NetException {
		return list.lookFor(job);
	}

	public boolean remove(JobInfo jobInfo) throws NetException {
		return list.remove(jobInfo);
	}

	public boolean removeAfterDrop(JobInfo jobInfo) throws NetException {
		return list.removeAfterDrop(jobInfo);
	}

	public boolean removeAfterRedirect(JobInfo jobInfo) throws NetException {
		return list.removeAfterRedirect(jobInfo);
	}

	public JobInfo removeFirst() throws NetException {
		return list.removeFirst();
	}

	public JobInfo removeFirst(JobClass jobClass) throws NetException {
		return list.removeFirst(jobClass);
	}

	public JobInfo removeLast() throws NetException {
		return list.removeLast();
	}

	public JobInfo removeLast(JobClass jobClass) throws NetException {
		return list.removeLast(jobClass);
	}

	public int size() {
		return size;
	}

	public int size(JobClass jobClass) {
		return sizePerClass[jobClass.getId()];
	}

	/* (non-Javadoc)
	 * @see jmt.engine.QueueNet.JobInfoList#setServerNumber(int)
	 */
	public void setServerNumber(int serverNumber) {
		this.serverNumber = serverNumber;
	}

	public void psJobIn(JobClass jobClass, double time) {
		size++;
		sizePerClass[jobClass.getId()]++;
		jobsIn++;
		jobsInPerClass[jobClass.getId()]++;
		lastJobInTime = time;
		lastJobInTimePerClass[jobClass.getId()] = time;
	}

	public void psJobOut(JobClass jobClass, double time) {
		size--;
		sizePerClass[jobClass.getId()]--;
		jobsOut++;
		jobsOutPerClass[jobClass.getId()]++;
		lastJobOutTime = time;
		lastJobOutTimePerClass[jobClass.getId()] = time;
	}

	public void psUpdateUtilization(JobClass jobClass, double multiplier, double time) {
		double lastModify = Math.max(getLastJobInTime(), getLastJobOutTime());
		double lastModifyPerClass = Math.max(getLastJobInTimePerClass(jobClass), getLastJobOutTimePerClass(jobClass));
		double value = size();
		double valuePerClass = size(jobClass);
		update(utilization, value * multiplier / serverNumber, time - lastModify);
		update(utilizationPerClass, jobClass, valuePerClass * multiplier / serverNumber, time - lastModifyPerClass);
		update(queueLength, value * multiplier, time - lastModify);
		update(queueLengthPerClass, jobClass, valuePerClass * multiplier, time - lastModifyPerClass);
	}

	public void psUpdateBusyTimes(JobClass jobClass, double busyTime) {
		int id = jobClass.getId();
		update(residenceTime, busyTime, 1.0);
		update(residenceTimePerClass, jobClass, busyTime, 1.0);
		update(responseTime, busyTime, 1.0);
		update(queueLengthPerClass, jobClass, busyTime, 1.0);
		this.busyTime += busyTime;
		busyTimePerClass[id] += busyTime;
	}

	public void psUpdateThroughput(JobClass jobClass) {
		update(throughput, NetSystem.getTime() - getLastJobOutTime(), 1.0);
		update(throughputPerClass, jobClass, NetSystem.getTime() - getLastJobOutTimePerClass(jobClass), 1.0);
	}

	/**
	 * Updates a measure if it is not null
	 * @param m the mesure, may be null
	 * @param value the value
	 * @param weight the weight
	 */
	private void update(Measure m, double value, double weight) {
		if (m != null) {
			m.update(value, weight);
		}
	}

	/**
	 * Updates a perclass measure if it is not null
	 * @param m the mesure, may be null
	 * @param value the value
	 * @param weight the weight
	 */
	private void update(Measure[] m, JobClass jobClass, double value, double weight) {
		if (m != null && m[jobClass.getId()] != null) {
			m[jobClass.getId()].update(value, weight);
		}
	}

	/**
	 * @return the internal jobInfoList to be used by Queue class.
	 */
	public JobInfoList getInternalList() {
		return list;
	}
	
	public void analyzeResponseTimePerSink(JobClass jobClass,
			Measure Measurement) {
		list.analyzeResponseTimePerSink(jobClass, Measurement);
	}

	public void analyzeThroughputPerSink(JobClass jobClass,
			InverseMeasure Measurement) {
		list.analyzeThroughputPerSink(jobClass, Measurement);
	}

}
