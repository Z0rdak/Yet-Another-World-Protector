package de.z0rdak.yawp;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.Services;

// This class is part of the common project meaning it is shared between all supported loaders. Code written here can only
// import and access the vanilla codebase, libraries used by vanilla, and optionally third party libraries that provide
// common compatible binaries. This means common code can not directly use loader specific concepts such as Forge events
// however it will be compatible with all supported mod loaders.
public class YAWPCommon {

    // The loader specific projects are able to import and use any code from the common project. This allows you to
    // write the majority of your code here and load it from your loader specific projects. This example has some
    // code that gets invoked by the entry point of the loader specific projects.
    public static void init() {
        Constants.LOGGER.debug("[{}] Loading {} in a {} environment!", Constants.MOD_ID, Services.PLATFORM.getPlatformName(), Services.PLATFORM.getEnvironmentName());
        // It is common for all supported loaders to provide a similar feature that can not be used directly in the
        // common code. A popular way to get around this is using Java's built-in service loader feature to create
        // your own abstraction layer. You can learn more about this in our provided services class. In this example
        // we have an interface in the common code and use a loader specific implementation to delegate our call to
        // the platform specific approach.
        if (Services.PLATFORM.isModLoaded(Constants.MOD_ID)) {
            Constants.LOGGER.debug("Hi, {}! Great you have you with us! One of us, one of us, one of us, ...", Constants.MOD_ID);
        }
    }
}