/**    
  * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */

package jmt.test.engine;

import java.util.ListIterator;

import jmt.common.exception.NetException;
import jmt.engine.NetStrategies.QueuePutStrategy;
import jmt.engine.NetStrategies.QueuePutStrategies.HeadStrategy;
import jmt.engine.NetStrategies.QueuePutStrategies.HeadStrategyPriority;
import jmt.engine.NetStrategies.QueuePutStrategies.TailStrategy;
import jmt.engine.NetStrategies.QueuePutStrategies.TailStrategyPriority;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.LinkedJobInfoList;

/**
 * Created by:
 * User: Stefano
 * Date: 6-mag-2005
 * Time: 16.12.15
 */
public class TestClassPriorityStrategy {

	private static final int STRATEGY_HEAD = 0;
	private static final int STRATEGY_TAIL = 1;
	private static final int STRATEGY_HEAD_PRIORITY = 2;
	private static final int STRATEGY_TAIL_PRIORITY = 3;

	public static void test1(int strategy) {
		JobClass class1, class2, class3;

		try {
			class1 = new JobClass("high  ", 3, JobClass.CLOSED_CLASS, null);
			class2 = new JobClass("medium", 2, JobClass.CLOSED_CLASS, null);
			class3 = new JobClass("low   ", 1, JobClass.CLOSED_CLASS, null);

		} catch (NetException ne) {
			ne.printStackTrace();
			return;
		}

		JobInfoList list = new LinkedJobInfoList(3, true);

		QueuePutStrategy putStrategy_H = new HeadStrategy();
		QueuePutStrategy putStrategy_T = new TailStrategy();
		QueuePutStrategy putStrategy_HP = new HeadStrategyPriority();
		QueuePutStrategy putStrategy_TP = new TailStrategyPriority();

		QueuePutStrategy putStrategy = null;

		switch (strategy) {
			case STRATEGY_HEAD:
				putStrategy = putStrategy_H;
				break;
			case STRATEGY_TAIL:
				putStrategy = putStrategy_T;
				break;
			case STRATEGY_HEAD_PRIORITY:
				putStrategy = putStrategy_HP;
				break;
			case STRATEGY_TAIL_PRIORITY:
				putStrategy = putStrategy_TP;
				break;
		}

		Job[] jobs = new Job[10];

		jobs[0] = new Job(class1);
		jobs[1] = new Job(class3);
		jobs[2] = new Job(class2);
		jobs[3] = new Job(class2);
		jobs[4] = new Job(class1);
		jobs[5] = new Job(class1);
		jobs[6] = new Job(class1);
		jobs[7] = new Job(class2);
		jobs[8] = new Job(class3);
		jobs[9] = new Job(class3);

		byte b = 0x01;

		for (Job job : jobs) {
			try {
				putStrategy.put(job, list, b, null, null);
			} catch (NetException ne) {
				ne.printStackTrace();
				return;
			}
		}

		//print
		ListIterator iterator = list.getJobList().listIterator();
		JobInfo current = null;
		String className;
		int classPr, jobID;

		String strategyType = "";

		switch (strategy) {
			case STRATEGY_HEAD:
				strategyType = "HEAD";
				break;
			case STRATEGY_TAIL:
				strategyType = "TAIL";
				break;
			case STRATEGY_HEAD_PRIORITY:
				strategyType = "HEAD_PR";
				break;
			case STRATEGY_TAIL_PRIORITY:
				strategyType = "TAIL_PR";
				break;
		}

		System.out.println(strategyType);

		//iterator starts from the first (i.e. the job with highest priority)
		while (iterator.hasNext()) {
			current = (JobInfo) iterator.next();

			jobID = current.getJob().getId();
			className = current.getJob().getJobClass().getName();
			classPr = current.getJob().getJobClass().getPriority();

			System.out.println("Job: " + jobID + " Class: " + className + " Priority: " + classPr);

		}

		System.out.println();

	}

	public static void main(String[] args) {

		test1(STRATEGY_HEAD);
		System.out.println();
		System.out.println();
		test1(STRATEGY_TAIL);
		System.out.println();
		System.out.println();
		test1(STRATEGY_HEAD_PRIORITY);
		System.out.println();
		System.out.println();
		test1(STRATEGY_TAIL_PRIORITY);

	}
}
