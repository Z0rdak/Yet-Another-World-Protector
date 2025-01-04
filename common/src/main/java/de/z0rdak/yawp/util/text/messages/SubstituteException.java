package de.z0rdak.yawp.util.text.messages;

import java.util.Locale;

public class SubstituteException
        extends IllegalArgumentException {
    public SubstituteException(SubstituteTextContent text, String message) {
        super(String.format(Locale.ROOT, "Error parsing: %s: %s", text, message));
    }

    public SubstituteException(SubstituteTextContent text, int index) {
        super(String.format(Locale.ROOT, "Invalid index %d requested for %s", index, text));
    }

    public SubstituteException(SubstituteTextContent text, Throwable cause) {
        super(String.format(Locale.ROOT, "Error while parsing: %s", text), cause);
    }
}
