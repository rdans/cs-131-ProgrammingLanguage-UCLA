import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private byte maxval;
    private AtomicIntegerArray value;

    private void AtomicArray(byte[] v){
        int[] int_a = new int[v.length];
        for(int i = 0; i < v.length; i++){
            int_a[i] = v[i];
        }
        value = new AtomicIntegerArray(int_a);
    }

    GetNSet(byte[] v) { maxval = 127; AtomicArray(v); }

    GetNSet(byte[] v, byte m) { maxval = m; AtomicArray(v); }

    public int size() { return value.length(); }

    public byte[] current() {
        byte[] a_byte = new byte[value.length()];
        for(int i = 0; i < a_byte.length; i++){
            a_byte[i] = (byte) value.get(i);
        }
        return a_byte;
    }

    public boolean swap(int i, int j) {
        if (value.get(i) <= 0 || value.get(j) >= maxval)
            return false;

        value.set(i, value.get(i) - 1);
        value.set(j, value.get(j) + 1);
        return true;
    }


}