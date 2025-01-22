package de.z0rdak.yawp.external;

import de.z0rdak.yawp.api.events.region.RegionEvent;

import java.util.LinkedList;
import java.util.List;

/**
 * Singleton to register extensions for mod integrations and notify them on certain events as observers.
 */
public class WebMapRegistry {
    private final List<WebMapIntegration> extensions = new LinkedList<>();

    public WebMapRegistry() {
    }

    public void initialize() {
    }

    void register(WebMapIntegration extension) {
        extensions.add(extension);
    }

    public boolean notify(RegionEvent.Create event) {
        extensions.forEach(ext -> ext.on(event));
        return true;
    }

    public boolean notify(RegionEvent.Remove event) {
        extensions.forEach(ext -> ext.on(event));
        return true;
    }

    public boolean notify(RegionEvent.Rename event) {
        extensions.forEach(ext -> ext.on(event));
        return true;
    }

    public boolean notify(RegionEvent.UpdateArea event) {
        extensions.forEach(ext -> ext.on(event));
        return true;
    }
}
