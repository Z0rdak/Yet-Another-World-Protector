package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.entity.Entity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.mob.*;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.MerchantEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.registry.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.config.server.CommandPermissionConfig.hasRegionPermission;
import static de.z0rdak.yawp.util.MessageSender.sendDimFlagNotification;
import static de.z0rdak.yawp.util.MessageSender.sendFlagNotification;

public final class HandlerUtil {

    private HandlerUtil() {
    }

    public static RegistryKey<World> getEntityDim(Entity entity) {
        return entity.getWorld().getRegistryKey();
    }

    public static boolean isAnimal(Entity entity) {
        return entity instanceof AnimalEntity || entity instanceof WaterCreatureEntity;
    }

    public static boolean isServerSide(Entity entity) {
        return !entity.getWorld().isClient;
    }

    public static boolean isServerSide(World world) {
        return !world.isClient;
    }

    public static boolean isVillager(Entity entity) {
        return entity instanceof MerchantEntity;
    }

    public static boolean isPlayer(Entity entity) {
        return entity instanceof PlayerEntity;
    }

    public static boolean isMonster(Entity entity) {
        return entity instanceof HostileEntity
                || entity instanceof SlimeEntity
                || entity instanceof FlyingEntity
                || entity instanceof EnderDragonEntity
                || entity instanceof ShulkerEntity;
    }

    /**
     * Processes the given flag check event and executes the given consumers if the flag is allowed or denied. <br>
     * The flag check event is evaluated and posted to the event bus. <br>
     * If the flag is allowed, the onAllow consumer is executed, otherwise onDeny. <br>
     *
     * @param checkEvent the flag check event to process
     * @param onAllow    the consumer to execute if the flag is allowed
     * @param onDeny     the consumer to execute if the flag is denied
     * @return the flag state of the result
     */
    public static FlagState processCheck(FlagCheckEvent checkEvent, @Nullable Consumer<FlagCheckResult> onAllow, @Nullable Consumer<FlagCheckResult> onDeny) {
        FlagCheckResult result = evaluate(checkEvent);
        result = RegionEvents.FLAG_RESULT.invoker().getResult(result);
        if (result.getFlagState() == FlagState.ALLOWED
                || result.getFlagState() == FlagState.UNDEFINED
                || result.getFlagState() == FlagState.DISABLED) {
            if (onAllow != null)
                onAllow.accept(result);
        }
        if (result.getFlagState() == FlagState.DENIED) {
            if (onDeny != null)
                onDeny.accept(result);
        }
        return result.getFlagState();
    }

    /**
     * Evaluates the given flag check event and returns the result. <br>
     * The responsible region is determined and the flag state is checked against the player's permissions. <br>
     *
     * @param checkEvent the flag check event to evaluate
     * @return the result of the flag check event
     */
    public static FlagCheckResult evaluate(FlagCheckEvent checkEvent) {
        RegionFlag regionFlag = checkEvent.getRegionFlag();
        IProtectedRegion targetRegion = getResponsible(checkEvent.getTarget(), checkEvent.getDimension());
        if (targetRegion == null) {
            return new FlagCheckResult(checkEvent, FlagState.UNDEFINED, null, null);
        }
        FlagCorrelation responsibleFlag = getResponsibleFlag(targetRegion, regionFlag, null);
        FlagState playerRelatedState = getFlagState(responsibleFlag.getRegion(), regionFlag, checkEvent.getPlayer());
        return new FlagCheckResult(checkEvent, playerRelatedState, responsibleFlag.getRegion(), responsibleFlag.getFlag());
    }

    /**
     * Gets the responsible region for the given position and dimension. <br>
     * The responsible region is the region with the highest priority among all involved regions at the given location and dimension. <br>
     * If no Local Region is defined at the given position, the dimensional region is responsible and returned. <br>
     *
     * @param pos the position to get the responsible region for
     * @param dim the dimension to get the responsible region for
     * @return the responsible region for the given position and dimension
     */
    @Nullable
    private static IProtectedRegion getResponsible(BlockPos pos, RegistryKey<World> dim) {
        IMarkableRegion region = getInvolvedRegionFor(pos, dim);
        if (region == null) {
            IProtectedRegion dimRegion = RegionDataManager.get().cacheFor(dim).getDimensionalRegion();
            if (dimRegion.isActive()) {
                return dimRegion;
            } else {
                return RegionDataManager.get().getGlobalRegion().isActive()
                        ? RegionDataManager.get().getGlobalRegion()
                        : null;
            }
        }
        return region;
    }

    /**
     * Gets all active regions which contain the provided position in the given dimension. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return all active regions which contain the given location and dimension
     */
    private static List<IMarkableRegion> getInvolvedRegionsFor(BlockPos position, RegistryKey<World> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    /**
     * Gets the region with the highest priority among all involved regions at the given location and dimension. <br>
     * This considers the active state of the region as well. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return the region with the highest priority among all involved regions which contain the given location
     */
    @Nullable
    private static IMarkableRegion getInvolvedRegionFor(BlockPos position, RegistryKey<World> dim) {
        List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(position, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    /**
     * Gets the flag state for the given region and flag. <br>
     * If the player is null, the flag state is returned as is. <br>
     * If the player is not null, the flag state is checked against the player's permissions. <br>
     * If the player is permitted, the flag state is returned as allowed. <br>
     *
     * @param region the region to get the flag state for
     * @param flag   the flag to get the state for
     * @param player the player to check the flag state against
     * @return the flag state for the given region and flag
     */
    private static FlagState getFlagState(IProtectedRegion region, RegionFlag flag, @Nullable PlayerEntity player) {
        if (player == null) {
            return region.getFlagContainer().flagState(flag.name);
        } else {
            boolean hasPermission = hasRegionPermission(region, player, CommandUtil.OWNER) || hasRegionPermission(region, player, CommandUtil.MEMBER);
            boolean isPermitted = hasPermission || CommandPermissionConfig.hasConfigPermAndOpByPassFlags(player);
            if (isPermitted) {
                return FlagState.ALLOWED;
            } else {
                return region.getFlagContainer().flagState(flag.name);
            }
        }
    }

    public static Map<String, FlagCorrelation> getFlagMapRecursive(IProtectedRegion region, Map<String, FlagCorrelation> carry) {
        if (carry == null) {
            carry = region.getFlagContainer().entrySet().stream()
                    .filter(flag -> flag.getValue().getState() != FlagState.UNDEFINED)
                    .collect(Collectors.toMap(Map.Entry::getKey, entry -> new FlagCorrelation(region, entry.getValue())));
        }
        if (region.equals(region.getParent())) {
            // global region has itself as parent
            Set<Map.Entry<String, IFlag>> flags = getNonUndefinedFlags(region);
            for (Map.Entry<String, IFlag> entry : flags) {
                if (!carry.containsKey(entry.getKey())) {
                    carry.put(entry.getValue().getName(), new FlagCorrelation(region, entry.getValue()));
                }
            }
            return carry;
        }
        Set<Map.Entry<String, IFlag>> parentFlags = getNonUndefinedFlags(region.getParent());
        for (Map.Entry<String, IFlag> entry : parentFlags) {
            if (!carry.containsKey(entry.getKey())) {
                carry.put(entry.getValue().getName(), new FlagCorrelation(region.getParent(), entry.getValue()));
            }
            if (entry.getValue().doesOverride()) {
                carry.put(entry.getValue().getName(), new FlagCorrelation(region.getParent(), entry.getValue()));
            }
        }
        return getFlagMapRecursive(region.getParent(), carry);
    }

    private static Set<Map.Entry<String, IFlag>> getNonUndefinedFlags(IProtectedRegion region) {
        return region.getFlagContainer().entrySet().stream()
                .filter(flag -> flag.getValue().getState() != FlagState.UNDEFINED)
                .collect(Collectors.toSet());
    }


    /**
     * Recursively gets all flags defined in the region hierarchy of the provided region, including all it's parents. <br></br>
     * This is used, when we have determined the responsible region for an Event or a BlockPos. <br></br>
     * We need to check all flags of the responsible region and all its parents to determine the final flag state. <br></br>
     *
     * @param region the region (and its parents) to get active flags
     * @param carry  a flag container, holding information about region flags
     * @return a flag container of all active flags of the given region including its parents.
     */
    public static FlagContainer getFlagsRecursive(IProtectedRegion region, FlagContainer carry) {
        if (region.equals(region.getParent())) { // global region has itself as parent
            return carry == null ? region.getFlagContainer().deepCopy() : carry;
        }
        if (carry == null) { // effectively make a deep copy of the flag container
            carry = region.getFlagContainer().deepCopy();
        }
        Map<String, IFlag> activeParentFlags = region.getParent().getFlagContainer().getActiveFlags();
        for (Map.Entry<String, IFlag> entry : activeParentFlags.entrySet()) {
            String flagName = entry.getKey();
            IFlag flag = entry.getValue();
            boolean parentFlagOverrides = flag.doesOverride();
            boolean existingFlag = carry.contains(flagName);
            if (parentFlagOverrides && existingFlag) {
                carry.remove(flagName);
                carry.put(flagName, flag);
            }
            if (!existingFlag) {
                carry.put(flagName, flag);
            }
        }
        return getFlagsRecursive(region.getParent(), carry);
    }

    /**
     * Recursively gets the region hierarchy for the given region. <br>
     * The region hierarchy is a list of regions starting with the given region and ending with the global region. <br>
     *
     * @param region the region to get the hierarchy for
     * @param carry  the list to carry the hierarchy (initially empty)
     * @return the region hierarchy for the given region
     */
    public static List<IProtectedRegion> getRegionHierarchy(IProtectedRegion region, List<IProtectedRegion> carry) {
        if (region.equals(region.getParent())) { // global region has itself as parent
            return carry.isEmpty() ? Collections.singletonList(region) : carry;
        }
        carry.add(region);
        return getRegionHierarchy(region.getParent(), carry);
    }

    /**
     * Recursively gets the flag correlation for the given region and flag. <br>
     * The flag correlation is the region and flag pair which is responsible for the flag state. <br>
     * The flag correlation is determined by checking the flag state of the region and its parents. <br>
     * If the flag is allowed or denied, the flag correlation is set. <br>
     * If the flag is undefined, the parent region is checked. <br>
     * If the parent region is the global region, the flag correlation is returned. <br>
     *
     * @param region     the region to get the flag correlation for
     * @param regionFlag the region flag to get the correlation for
     * @param carry      the flag correlation to carry (initially null)
     * @return the flag correlation for the given region and flag
     */
    private static FlagCorrelation getFlagCorrelation(IProtectedRegion region, RegionFlag regionFlag, @Nullable FlagCorrelation carry) {
        if (region.equals(region.getParent())) {
            if (region.getFlagContainer().flagState(regionFlag.name) != FlagState.UNDEFINED) {
                IFlag flag = region.getFlag(regionFlag.name);
                if (flag.doesOverride()) {
                    carry = new FlagCorrelation(region, flag);
                }
            }
            return carry;
        }
        FlagState flagState = region.getFlagContainer().flagState(regionFlag.name);
        if (flagState != FlagState.UNDEFINED) {
            // allowed or denied
            carry = new FlagCorrelation(region, region.getFlag(regionFlag.name));
        }
        return getFlagCorrelation(region.getParent(), regionFlag, carry);
    }

    public static FlagCorrelation getResponsibleFlag(IProtectedRegion region, RegionFlag regionFlag, @Nullable FlagCorrelation carry) {
        if (carry == null) {
            FlagState flagState = region.getFlagContainer().flagState(regionFlag.name);
            if (flagState == FlagState.ALLOWED || flagState == FlagState.DENIED) {
                IFlag flag = region.getFlag(regionFlag.name);
                carry = new FlagCorrelation(region, flag);
            } else
                carry = new FlagCorrelation(region, null);
        }
        if (region.equals(region.getParent())) {
            if (region.getFlagContainer().flagState(regionFlag.name) != FlagState.UNDEFINED) {
                if (carry.getFlag() == null) {
                    carry = new FlagCorrelation(region, region.getFlag(regionFlag.name));
                }
            }
            return carry;
        }
        FlagState flagState = region.getParent().getFlagContainer().flagState(regionFlag.name);
        if (flagState == FlagState.ALLOWED || flagState == FlagState.DENIED) {
            IFlag flag = region.getParent().getFlag(regionFlag.name);
            if (carry.getFlag() == null) {
                carry = new FlagCorrelation(region.getParent(), flag);
            }
            if (flag.doesOverride()) {
                carry = new FlagCorrelation(region.getParent(), flag);
            }
        }
        return getResponsibleFlag(region.getParent(), regionFlag, carry);
    }
}
