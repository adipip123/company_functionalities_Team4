package p1;

import java.util.*;
import java.util.Iterator;

public class EmployeeManagement {
	public static void main(String[] args) {
		Scanner sc =new Scanner(System.in);
		boolean flag=false;
		int count=1;
		ArrayList<Employee> ar1=new ArrayList<Employee>();
		Employee e1=new EmployeeBuilder().setAge(20).setName("Aaron").setGender("male").setProjectid(1).setSsoid(212613066).getEmployee();
		Employee e2=new EmployeeBuilder().setGender("female").setProjectid(3).setSsoid(926382922).setAge(25).setName("Vijay").getEmployee();
		Employee e3=new EmployeeBuilder().setAge(31).setName("Mallya").setGender("male").setSsoid(936194629).getEmployee();
		Employee e4=new EmployeeBuilder().setAge(60).setName("Aditi").setGender("female").setProjectid(1).setSsoid(645281936).getEmployee();
		Employee e5=new EmployeeBuilder().setName("Meghna").setProjectid(4).setGender("female").setSsoid(471927552).getEmployee();

        ar1.add(e1);
        ar1.add(e2);
        ar1.add(e3);
        ar1.add(e4);
        ar1.add(e5);
        System.out.println("The List of existing employees are:");
        Iterator<Employee> itr= ar1.iterator();
        while(itr.hasNext()){
        	System.out.println(itr.next());
        }
		
        //employee value updation
        System.out.println();
        System.out.println("Do you want to make any changes in the database? Y/N");
        //sc.nextLine();
        if(sc.nextLine().equalsIgnoreCase("Y"))
        {
	        while(true){
	        System.out.println("Enter the Ssoid of the employee who's record has to be updated.");
	        int updationId =sc.nextInt();
	        Iterator<Employee> itr2= ar1.iterator();
	        while(itr2.hasNext())
	        {	Employee p=itr2.next();
	        	if(updationId==p.getSsoid())
	        	{
	        		flag=true;
	        		System.out.println("enter the field you want to update");
	        		sc.nextLine();
	        		String s=sc.nextLine();
	        		p.match(s);
	        		System.out.println(p);	
	        	}
	        }
	        if(!flag)
	        	System.out.println("Ssoid not found.");
	        System.out.println("Do you want to update again?Y/N.");
	        sc.nextLine();
	        if(sc.nextLine().equalsIgnoreCase("N"))
	        	break;
	        }
        }
        System.out.println();
         
        
        
        
        System.out.println("Following are the existing version available:");
        System.out.println();
        //memento pattern begins here
        Originator o1=new Originator();
        Caretaker c1=new Caretaker();
        ArrayList<String> a1=new ArrayList<String>();
        
        a1.add("level 1");
        a1.add("level 2");
        a1.add("level 3");
        o1.setState(a1);
        c1.add(o1.saveStateToMemento());
        
        ArrayList<String> a2=new ArrayList<String>();
		a2.add("Software Analyst");
		a2.add("Senior Software Analyst");
		a2.add("Managing director");
		a2.add("Ceo");
		a2.add("VP");
        o1.setState(a2); 
        c1.add(o1.saveStateToMemento());
        
        ArrayList<String> a3=new ArrayList<String>();
        a3.add("Design Engineer");
        a3.add("Staff Engineer");
        a3.add("Training Engineer");
        a3.add("Design Engineer");
        a3.add("Hacking Engineer");
        a3.add("Arts and Crafts Engineer");
        a3.add("Manager");
        o1.setState(a3);
        c1.add(o1.saveStateToMemento());
        
       
        //showing different states
        Iterator i=c1.getversions().iterator();
        
       while(i.hasNext())
       {    
    	   System.out.println("Version number: "+count++);
    	   
    	   Memento a=(Memento)i.next();
    	   ArrayList list=a.getState();
    	   for(Object s1:list)
    	   {
    		   System.out.println(s1);
    	   }
    	 System.out.println();
    	   //   Iterator<String> itr3= list.iterator();
           //while(itr3.hasNext()){
           //	System.out.println(itr3.next());
           //}  
    	   
       }
        
       
       	//version control
        System.out.println("Enter the version for hierarchy you prefer");
        while(true)
        {
         int option =sc.nextInt();
         	if(option>c1.getSize())
         		System.out.println("please select valid options");
         	else
         	{
         		System.out.println("Restoring the version number "+option);
         		o1.getStateFromMemento(c1.getPastState(option-1)); 
         		Iterator<ArrayList> itr1= o1.getState().iterator();
         		System.out.println("The restored version is: ");
         			while(itr1.hasNext())
         			{
         					System.out.println(itr1.next());
         			}
        	break;
            }
        
        }
	}
}
