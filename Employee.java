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
	public String toString(){
	// return "Ssoid:"+ssoid+",   Name: "+name+",   Gender: "+gender+",   Age: "+age+",   ProjectId: "+projectid+" ";
		String s=String.format("Ssoid: %d, Name: %-8s, Gender: %-6s, Age: %-3d, ProjectId: %-4d",ssoid,name,gender,age,projectid);
		return s;
	}
	
	
}
