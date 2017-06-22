package p1;
import java.util.ArrayList;

public class Caretaker {
	private ArrayList<Memento> projStatus= new ArrayList<Memento>();
	
	public void add(Memento m){
		projStatus.add(m);
	}
	
	public Memento getPastState(int index){
		return projStatus.get(index);
	}
}
