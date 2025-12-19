package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.services.FeatureManager;
import de.z0rdak.yawp.platform.services.IPermissionHelper;
import de.z0rdak.yawp.platform.services.IPlatformHelper;
import de.z0rdak.yawp.platform.services.config.IFlagConfigHelper;
import de.z0rdak.yawp.platform.services.config.ILoggingConfigHelper;
import de.z0rdak.yawp.platform.services.config.IPermissionConfigHelper;
import de.z0rdak.yawp.platform.services.config.IRegionConfigHelper;
import de.z0rdak.yawp.platform.services.event.FlagEventDispatcher;
import de.z0rdak.yawp.platform.services.event.RegionEventDispatcher;
import de.z0rdak.yawp.platform.services.event.YawpEventDispatcher;

import java.util.ServiceLoader;

public class Services {

    public static final IPlatformHelper PLATFORM = load(IPlatformHelper.class);
    public static final IPermissionConfigHelper PERMISSION_CONFIG = load(IPermissionConfigHelper.class);
    public static final IFlagConfigHelper FLAG_CONFIG = load(IFlagConfigHelper.class);
    public static final IRegionConfigHelper REGION_CONFIG = load(IRegionConfigHelper.class);
    public static final ILoggingConfigHelper LOGGING_CONFIG = load(ILoggingConfigHelper.class);
    public static final IPermissionHelper PERMISSIONS = load(IPermissionHelper.class);
    public static final FlagEventDispatcher FLAG_EVENT_DISPATCHER = Services.load(FlagEventDispatcher.class);
    public static final RegionEventDispatcher REGION_EVENT_DISPATCHER = Services.load(RegionEventDispatcher.class);
    public static final YawpEventDispatcher YAWP_EVENT_DISPATCHER = Services.load(YawpEventDispatcher.class);
    public static final FeatureManager FEATURE_MANAGER = Services.load(FeatureManager.class);

    public static <T> T load(Class<T> clazz) {

        final T loadedService = ServiceLoader.load(clazz)
                .findFirst()
                .orElseThrow(() -> {
                    Constants.LOGGER.error("Unable to load service {}", clazz.getSimpleName());
                    return new NullPointerException("Failed to load service for " + clazz.getName());
                });
        Constants.LOGGER.debug("Loaded {} for service {}", loadedService, clazz);
        return loadedService;
    }
}