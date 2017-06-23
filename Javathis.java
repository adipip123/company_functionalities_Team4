package p1;

public class Javathis {
	void m(){  
		System.out.println(this);//prints same reference ID  
		}  


	public static void main(String args[])
	{
		
			
			Javathis obj=new Javathis();  
			System.out.println(obj);//prints the reference ID  
			obj.m();  
			}  
		 
	
	
}
//op-p1.Javathis@2a139a55
//p1.Javathis@2a139a55