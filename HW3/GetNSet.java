import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State{
	private AtomicIntegerArray value;
	private byte maxval;
	
	GetNSet(byte[] v) {
		int l = v.length;
		int[] int_n = new int[l];
		for (int i = 0; i < l; i++)
			int_n[i] = v[i];
		value = new AtomicIntegerArray(int_n);
		maxval = 127;
		}
	
	GetNSet(byte[] v, byte m){
		int l = v.length;
		int[] int_n = new int[l];
		for (int i = 0; i < l; i++)
			int_n[i] = v[i];
		value = new AtomicIntegerArray(int_n);
		maxval = m;
	}
	
	public int size() {return value.length();}
	
	public byte[] current() {
		int l = value.length();
		byte[] byte_n = new byte[l];
		for (int i = 0; i < l; i++)
			byte_n[i] = (byte) value.get(i);
		return byte_n;
	}
	
	public boolean swap (int i, int j){
		if (value.get(i) <= 0 || value.get(j) >= maxval)
			return false;
		value.set(i, value.get(i)-1);
		value.set(j, value.get(j)+1);
		return true;	
	}
}
