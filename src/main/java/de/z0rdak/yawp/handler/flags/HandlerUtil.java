package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.*;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.entity.Entity;
import net.minecraft.entity.FlyingEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.merchant.villager.AbstractVillagerEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.monster.ShulkerEntity;
import net.minecraft.entity.monster.SlimeEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.WaterMobEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.core.flag.FlagMessage.REGION_TEMPLATE;

public final class HandlerUtil {

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
        MinecraftForge.EVENT_BUS.post(result);
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

    private HandlerUtil(){}

    public static RegistryKey<World> getEntityDim(Entity entity){
        return entity.getCommandSenderWorld().dimension();
    }

    public static boolean isAnimal(Entity entity){
        return entity instanceof AnimalEntity || entity instanceof WaterMobEntity;
    }

    public static boolean isServerSide(EntityEvent event) {
        return isServerSide(event.getEntity());
    }

    public static boolean isServerSide(BlockEvent event) {
        return !event.getWorld().isClientSide();
    }

    public static boolean isServerSide(Entity entity) {
        return !entity.getCommandSenderWorld().isClientSide;
    }

    public static boolean isVillager(Entity entity) {
        return entity instanceof AbstractVillagerEntity;
    }

    public static boolean isPlayer(Entity entity) {
        return entity instanceof PlayerEntity;
    }

    public static boolean isMonster(Entity entity) {
        return entity instanceof MonsterEntity
                || entity instanceof SlimeEntity
                || entity instanceof FlyingEntity
                || entity instanceof EnderDragonEntity
                || entity instanceof ShulkerEntity;
    }

    /**
     * Sends the flag message for the given flag check event. <br>     *
     *
     * @param result the flag check event to send the message for
     */
    public static void sendFlagMsg(FlagCheckResult result) {
        IProtectedRegion responsibleRegion = result.getResponsible();
        IFlag flag = responsibleRegion.getFlag(result.getFlag().getName());
        boolean isFlagMuted = flag.getFlagMsg().isMuted();
        // If not muted and the event is a player event, send the message
        if (!isFlagMuted && result.getPlayer() != null && result.getPlayer() instanceof PlayerEntity) {
            Map<String, String> msgSubstitutes = FlagMessage.defaultSubstitutesFor(result);
            msgSubstitutes.put(REGION_TEMPLATE, responsibleRegion.getName());
            IFormattableTextComponent flagMsg = FlagMessage.buildFrom(result, msgSubstitutes);
            MessageUtil.sendNotification(result.getPlayer(), flagMsg);
        }
    }

    /*
     a method that checks an event with the specified flag rule set
     child region takes priority over parent region, if flag of child region is set and denied
     if flag of child region is set and allowed, the event is allowed and the flag of the child child is checked
     if flag of child region is not set, the flag of the parent region is checked
     if flag of parent region is set and denied, the event is denied
     if flag of parent region is set and allowed, the event is allowed
     if flag of parent region is not set, the event is allowed and the flag of the parent parent is checked until global region is reached
     only the local region can be null, if so, the check for the dim region and its parent is performed
     write a recursive method that checks the flag of the region and its parents
     */


    /**
     * Evaluates the given flag check event and returns the result. <br>
     *
     * @param checkEvent the flag check event to evaluate
     * @return the result of the flag check event
     */
    public static FlagCheckResult evaluate(FlagCheckEvent checkEvent) {
        BlockPos target = checkEvent.getTarget();
        PlayerEntity player = checkEvent.getPlayer();
        RegionFlag regionFlag = checkEvent.getRegionFlag();
        IProtectedRegion responsibleRegion = getResponsible(target, checkEvent.getDimension());
        FlagContainer flags = getFlagsRecursive(responsibleRegion, new FlagContainer());
        FlagState flagState = flags.flagState(regionFlag.name);
        if (flagState == FlagState.UNDEFINED) {
            return new FlagCheckResult(regionFlag, FlagState.UNDEFINED, target, responsibleRegion, player);
        } else {
            // now we know that there is a flag active (either allowed or disallowed,
            // but we need to check the correct region which is responsible,
            // because a parent could override the child
            FlagCorrelation flagCorrelation = getFlagCorrelation(responsibleRegion, regionFlag, new FlagCorrelation(responsibleRegion, null));
            FlagState state = getFlagState(flagCorrelation.region, regionFlag, player);
            return new FlagCheckResult(regionFlag, state, target, flagCorrelation.region, player);
        }
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
    private static IProtectedRegion getResponsible(BlockPos pos, RegistryKey<World> dim) {
        IMarkableRegion region = getInvolvedRegionFor(pos, dim);
        if (region == null) {
            return RegionDataManager.get().cacheFor(dim).getDimensionalRegion();
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

    public static FlagState getFlagState(BlockPos pos, RegistryKey<World> dim, RegionFlag flag, @Nullable PlayerEntity player) {
        IProtectedRegion region = getResponsible(pos, dim);
        return getFlagState(region, flag, player);
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
            boolean isPermitted = region.permits(player);
            if (isPermitted) {
                return FlagState.ALLOWED;
            } else {
                return region.getFlagContainer().flagState(flag.name);
            }
        }
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
            return carry == null ? region.getFlagContainer() : carry;
        }
        Map<String, IFlag> activeParentFlags = region.getParent().getFlagContainer().getActiveFlags();
        activeParentFlags.forEach((flagName, flag) -> {
            boolean parentFlagOverrides = flag.doesOverride();
            boolean existingFlag = carry.contains(flagName);
            if (parentFlagOverrides && existingFlag) {
                carry.remove(flagName);
                carry.put(flagName, flag);
            }
        });
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
        if (flagState == FlagState.UNDEFINED) {
            return getFlagCorrelation(region.getParent(), regionFlag, carry);
        } else {
            // allowed or denied
            carry = new FlagCorrelation(region, region.getFlag(regionFlag.name));
            return getFlagCorrelation(region.getParent(), regionFlag, carry);
        }
    }

    /**
     * Processes the given flag check event and executes the given consumers if the flag is allowed or denied. <br>
     * The flag check event is evaluated and posted to the event bus. <br>
     * If the flag is allowed, the onAllow consumer is executed, otherwise onDeny. <br>
     *
     * @param event      the original related event
     * @param checkEvent the flag check event to process
     * @param onAllow    the consumer to execute if the flag is allowed
     * @param onDeny     the consumer to execute if the flag is denied
     * @return the flag state of the result
     */
    public static FlagState processCheck(Event event, FlagCheckEvent checkEvent, @Nullable BiConsumer<Event, FlagCheckResult> onAllow, @Nullable BiConsumer<Event, FlagCheckResult> onDeny) {
        FlagCheckResult result = evaluate(checkEvent);
        MinecraftForge.EVENT_BUS.post(result);
        if (result.getFlagState() == FlagState.ALLOWED
                || result.getFlagState() == FlagState.UNDEFINED
                || result.getFlagState() == FlagState.DISABLED) {
            if (onAllow != null)
                onAllow.accept(event, result);
        }
        if (result.getFlagState() == FlagState.DENIED) {
            if (onDeny != null)
                onDeny.accept(event, result);
        }
        return result.getFlagState();
    }

    /**
     * A correlation of a region and a flag. <br>
     * This is used to determine the responsible region for a flag state.
     */
    static class FlagCorrelation {

        public IProtectedRegion region;
        public IFlag flag;

        public FlagCorrelation(IProtectedRegion region, IFlag flag) {
            this.region = region;
            this.flag = flag;
        }
    }
}
