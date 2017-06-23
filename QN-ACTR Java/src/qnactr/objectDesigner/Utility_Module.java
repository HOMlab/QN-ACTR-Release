package qnactr.objectDesigner;

import java.util.*;

public class Utility_Module {
  
  public double alpha;
  public double dat;
  public double egs;
  public double iu;
  public double nu;
  public Hashtable Obsolete_Rule_Name_Origin_For_Multitasking=new Hashtable();
  public Hashtable pg_c=new Hashtable();
  public Hashtable PG_C_c=new Hashtable();
  public Hashtable PG_C_effort_rule_firing_time=new Hashtable();
  public Hashtable PG_C_efforts_for_cost_calculation=new Hashtable();
  public Hashtable PG_C_failure_flags=new Hashtable();
  public Hashtable PG_C_failures_num_for_each_rule=new Hashtable();
  public double PG_C_g;
  public double PG_C_ie;
  public double PG_C_initial_efforts_for_cost_calculation;
  public double PG_C_initial_failures_num;
  public double PG_C_initial_successes_num;
  public Hashtable PG_C_p=new Hashtable();
  public String PG_C_pl;
  public Hashtable PG_C_prior_c=new Hashtable();
  public Hashtable PG_C_prior_p=new Hashtable();
  public Hashtable PG_C_success_flags=new Hashtable();
  public Hashtable PG_C_successes_num_for_each_rule=new Hashtable();
  public Hashtable PG_C_value=new Hashtable();
  public Hashtable reward=new Hashtable();
  public Hashtable U_N_Without_Noise=new Hashtable();
  public boolean ul;
  public String ut;
  public Hashtable utility=new Hashtable();
  public String utility_Computation_Method = "";
  
  public Utility_Module(){
    alpha=0.2;
    dat=0.05;
    egs=0.0;
    iu=0.0;
    nu=0.0;
    PG_C_g=20.0;
    PG_C_ie=10.0;
    PG_C_initial_efforts_for_cost_calculation=0.05;
    PG_C_initial_failures_num=0.0;
    PG_C_initial_successes_num=1.0;
    PG_C_pl="nil";
    ul=false;
    ut="nil";
  }
}

