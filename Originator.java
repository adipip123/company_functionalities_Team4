package p1;
import java.util.*;
public class Originator {
	ArrayList<String> a;
	public void setState(ArrayList a){
		this.a=a;
	}
	public ArrayList getState(){
		return a;		
	}
	public Memento saveStateToMemento()	{ 
		return new Memento(a);
	}
	public void getStateFromMemento(Memento m){
		a=m.getState();
	}
}