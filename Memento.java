package p1;
import java.util.*;
public class Memento {
	ArrayList a;
	public Memento(ArrayList a){
		this.a=a;
	}
	public ArrayList getState(){
		return a;
	}	
}