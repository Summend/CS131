import java.util.concurrent.atomic.AtomicInteger;

class BetterSorry implements State{
	private AtomicInteger[] value;
	private byte maxval;
	
	BetterSorry(byte[] v) {
		int l = v.length;
		value = new AtomicInteger[l];
		for (int i = 0; i < l; i++)
			value[i] = new AtomicInteger(v[i]);
		maxval = 127;
		}
	
	BetterSorry(byte[] v, byte m){
		int l = v.length;
		value = new AtomicInteger[l];
		for (int i = 0; i < l; i++)
			value[i] = new AtomicInteger(v[i]);
		maxval = m;
	}
	
	public int size() {return value.length;}
	
	public byte[] current() {
		int l = value.length;
		byte[] byte_n = new byte[l];
		for (int i = 0; i < l; i++)
			byte_n[i] = (byte) value[i].intValue();
		return byte_n;
	}
	
	public boolean swap (int i, int j){
		if (value[i].get() <= 0 || value[j].get() >= maxval)
			return false;
		value[i].getAndDecrement();
		value[j].getAndIncrement();
		return true;	
	}
}
