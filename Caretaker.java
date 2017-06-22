package p1;
import java.util.ArrayList;
public class Caretaker {
	private ArrayList<Memento> version= new ArrayList<Memento>();
	public void add(Memento m){
		version.add(m);
	}
	public Memento getPastState(int index){
		return version.get(index);
	}
	public int getSize(){
		return version.size();
	}
	public ArrayList getversions()
	{
		return version;
	}
}