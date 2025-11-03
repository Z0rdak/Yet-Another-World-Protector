package de.z0rdak.yawp.platform.services.event;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public final class EventBus<T> {
    private final List<T> listeners = new ArrayList<>();

    public void register(T listener) {
        listeners.add(listener);
    }

    public void invoke(Consumer<T> invoker) {
        for (T listener : listeners) {
            invoker.accept(listener);
        }
    }
}