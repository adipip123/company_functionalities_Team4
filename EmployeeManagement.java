package p1;

import java.util.ArrayList;
import java.util.Iterator;

public class EmployeeManagement {

	public static void main(String[] args) {
		ArrayList<Employee> ar1=new ArrayList<Employee>();
		Employee e1=new EmployeeBuilder().setAge(20).setName("Aaron").setGender("male").setProjectid(1).setSsoid(212613066).getEmployee();
		Employee e2=new EmployeeBuilder().setGender("female").setProjectid(3).setSsoid(926382922).setAge(25).setName("Vijay").getEmployee();
		Employee e3=new EmployeeBuilder().setAge(31).setName("Mallya").setGender("male").setSsoid(936194629).getEmployee();
		Employee e4=new EmployeeBuilder().setAge(60).setName("Aditi").setGender("female").setProjectid(1).setSsoid(645281936).getEmployee();
		Employee e5=new EmployeeBuilder().setName("Meghna").setProjectid(4).setGender("female").setSsoid(471927552).getEmployee();
        
		//System.out.println(e1);
        ar1.add(e1);
        ar1.add(e2);
        ar1.add(e3);
        ar1.add(e4);
        ar1.add(e5);
        
        Iterator<Employee> itr= ar1.iterator();
        while(itr.hasNext()){
        	System.out.println(itr.next());
        }
				
        //memento pattern begins here
        Originator o1=new Originator();
        Caretaker c1=new Caretaker();
        
        o1.setState("Version 0.1");
        o1.setState("Version 0.2");
        c1.add(o1.saveStateToMemento());
        
        o1.setState("Version 1.1");
        c1.add(o1.saveStateToMemento());
        
        o1.setState("Version 1.2");
        System.out.println(o1.getState());
        
        o1.getStateFromMemento(c1.getPastState(1));
        System.out.println(o1.getState());
	}
}
