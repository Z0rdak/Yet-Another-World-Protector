package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.services.IConfigHelper;
import de.z0rdak.yawp.platform.services.IPermissionHelper;
import de.z0rdak.yawp.platform.services.config.IFlagConfigHelper;
import de.z0rdak.yawp.platform.services.config.ILoggingConfigHelper;
import de.z0rdak.yawp.platform.services.config.IPermissionConfigHelper;
import de.z0rdak.yawp.platform.services.IEventHelper;
import de.z0rdak.yawp.platform.services.IPlatformHelper;
import de.z0rdak.yawp.platform.services.config.IRegionConfigHelper;

import java.util.ServiceLoader;

// Service loaders are a built-in Java feature that allow us to locate implementations of an interface that vary from one
// environment to another. In the context of MultiLoader we use this feature to access a mock API in the common code that
// is swapped out for the platform specific implementation at runtime.
public class Services {

    // In this example we provide a platform helper which provides information about what platform the mod is running on.
    // For example this can be used to check if the code is running on Forge vs Fabric, or to ask the modloader if another
    // mod is loaded.
    public static final IPlatformHelper PLATFORM = load(IPlatformHelper.class);
    public static final IEventHelper EVENT = load(IEventHelper.class);
    public static final IPermissionConfigHelper PERMISSION_CONFIG = load(IPermissionConfigHelper.class);
    public static final IFlagConfigHelper FLAG_CONFIG = load(IFlagConfigHelper.class);
    public static final IRegionConfigHelper REGION_CONFIG = load(IRegionConfigHelper.class);
    public static final ILoggingConfigHelper LOGGING_CONFIG = load(ILoggingConfigHelper.class);
    public static final IPermissionHelper PERMISSIONS = load(IPermissionHelper.class);
    public static final IConfigHelper CONFIG_REGISTRY = load(IConfigHelper.class);
    
    // This code is used to load a service for the current environment. Your implementation of the service must be defined
    // manually by including a text file in META-INF/services named with the fully qualified class name of the service.
    // Inside the file you should write the fully qualified class name of the implementation to load for the platform. For
    // example our file on Forge points to ForgePlatformHelper while Fabric points to FabricPlatformHelper.
    public static <T> T load(Class<T> clazz) {

        final T loadedService = ServiceLoader.load(clazz)
                .findFirst()
                .orElseThrow(() -> new NullPointerException("Failed to load service for " + clazz.getName()));
        Constants.LOGGER.debug("Loaded {} for service {}", loadedService, clazz);
        return loadedService;
    }
}