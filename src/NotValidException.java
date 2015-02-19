import java.util.HashMap;

public class NotValidException extends Exception{
    public HashMap<String, Boolean> values;

    public NotValidException(HashMap<String, Boolean> values) {
        super();
        this.values = values;
    }
}
