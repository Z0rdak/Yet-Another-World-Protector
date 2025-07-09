package de.z0rdak.yawp.external;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.constants.Constants;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * Singleton to register extensions for mod integrations and notify them on certain events as observers.
 */
public class WebMapRegistry {
    private final List<WebMapIntegration> extensions = new LinkedList<>();

    public WebMapRegistry() {
    }

    public void initialize() {
        Map.<String, Supplier<WebMapInitializer>>of(
            "de.bluecolored.bluemap.api.BlueMapAPI", BlueMapIntegration::new,
            "org.dynmap.DynmapCommonAPIListener", () -> new DynMapIntegration() // Using method reference causes ClassLoader to already load DynmapCommonAPIListener
        ).forEach(this::initializeWebMapIntegration);
    }

    private void initializeWebMapIntegration(String className, Supplier<WebMapInitializer> initializer) {
        try {
            Class.forName(className, false, getClass().getClassLoader());
        } catch (ClassNotFoundException e) {
            Constants.LOGGER.debug("WebMap Integration could not be activated du to missing API: {}", className);
            return;
        }
        initializer.get().initialize(this);
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

    public void notifyOnLoad() {
        extensions.forEach(WebMapIntegration::onLoad);
    }
}
