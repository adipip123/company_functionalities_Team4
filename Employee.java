package p1;
import java.util.*;
public class Employee {

	private int ssoid;
	private String name;
	private String gender;
	private int age;
	private int projectid;
	
	public Employee(int ssoid, String name, String gender, int age, int projectid) {
		super();
		this.ssoid = ssoid;
		this.name = name;
		this.gender = gender;
		this.age = age;
		this.projectid = projectid;
	}
	public int getSsoid() {
		return ssoid;
	}
	public void setSsoid(int ssoid) {
		this.ssoid = ssoid;
	}
	public void setName(String name) {
		this.name = name;
	}
	public void setGender(String gender) {
		this.gender = gender;
	}
	public void setAge(int age) {
		this.age = age;
	}
	public void setProjectid(int projectid) {
		this.projectid = projectid;
	}
	public void match(String str)
	{
	     Scanner sc=new Scanner(System.in);	
		if(str.equalsIgnoreCase("ssoid"))
		{
			System.out.println("enter new ssoid");
			this.ssoid=sc.nextInt();
			
		}
		else if(str.equalsIgnoreCase("age"))
		{
			System.out.println("enter new age");
			this.age=sc.nextInt();
			
		}
		else if(str.equalsIgnoreCase("gender"))
		{
			System.out.println("enter new gender");
			this.gender=sc.nextLine();
			
		}
		else if(str.equalsIgnoreCase("projectid"))
		{
			System.out.println("enter new projectid");
			this.projectid=sc.nextInt();
			
		}
		else if(str.equalsIgnoreCase("name"))
		{
			System.out.println("enter new name");
			this.name=sc.nextLine();
			
		}
		else
			System.out.println("Not a valid field");		
	}

	
	public String toString(){
	// return "Ssoid:"+ssoid+",   Name: "+name+",   Gender: "+gender+",   Age: "+age+",   ProjectId: "+projectid+" ";
		String s=String.format("Ssoid: %d, Name: %-8s, Gender: %-6s, Age: %-3d, ProjectId: %-4d",ssoid,name,gender,age,projectid);
		return s;
	}
	
	
}
