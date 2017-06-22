package p1;

public class EmployeeBuilder {
	private int ssoid;
	private String name;
	private String gender;
	private int age;
	private int projectid;
	public EmployeeBuilder setSsoid(int ssoid) {
		this.ssoid = ssoid;
		return this;
	}
	public EmployeeBuilder setName(String name) {
		this.name = name;
		return this;
	}
	public EmployeeBuilder setGender(String gender) {
		this.gender = gender;
		return this;
	}
	public EmployeeBuilder setAge(int age) {
		this.age = age;
		return this;
	}
	public EmployeeBuilder setProjectid(int projectid) {
		this.projectid = projectid;
		return this;
	}
	
	public Employee getEmployee(){	
		return new Employee(ssoid,name,gender,age,projectid);
	}
}
