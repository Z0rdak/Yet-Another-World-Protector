package de.z0rdak.yawp.handler.flags;

/**
 * Contains event handler for flags not directly related to player actions.
 * E.g.
 */
public class WorldFlagHandler {

    private WorldFlagHandler() {
    }

    public static void registerEventHandler() {

    }
    /**
     * Prevents all lightning strikes to hurt entities
     * and removes the lightning entity itself (needs testing)
     * @param event information about the lightning striking an entity

    public static void onLightningStrikeOccur(EntityStruckByLightningEvent event){
    if (isServerSide(event)) {
    Entity poorEntity = event.getEntity();
    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(poorEntity));
    FlagCheckEvent flagCheckEvent = checkTargetEvent(poorEntity.blockPosition(), LIGHTNING_PROT, dimCache.getDimensionalRegion());
    event.setCanceled(flagCheckEvent.isDenied());
    if (flagCheckEvent.isDenied()) {
    event.getLightning().remove(Entity.RemovalReason.DISCARDED);
    return;
    }

    // TODO: Implement, flags not yet defined
    if (poorEntity instanceof PlayerEntity) {
    }
    if (poorEntity instanceof PigEntity) {
    }
    if (poorEntity instanceof CreeperEntity) {
    }
    if (poorEntity instanceof MooshroomEntity) { // Also check for entity data Type == red
    }
    if (poorEntity instanceof VillagerEntity) {
    }
    if (poorEntity instanceof SkeletonHorseEntity) {
    }
    }
    }

    /**
     * Prevents all nether portal spawning.
     * E.g. flint and steel, fire charge, ghast projectiles, dispenser + flint & steel, etc.
     * This has its uses for markable regions but has limited use for dimensional regions.
     * @param event containing information of nether portal to be created

    public static void onNetherPortalSpawn(BlockEvent.PortalSpawnEvent event) {
    World world = event.getWorld();
    if (isServerSide(event)) {
    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), SPAWN_PORTAL, dimCache.getDimensionalRegion());
    event.setCanceled(flagCheckEvent.isDenied());
    }
    }

    /**
     * Handler prevents entities from using portals to travel between dimensions.
     * This has its uses for markable regions but limited use for dimensional regions.
     * Note: This event is only fired for PlayerEntity (1.16.5), See mixins for other entities.
     *
     * @param event holding info about the entity traveling from one to another dimension.

    public static void onUsePortal(EntityTravelToDimensionEvent event) {
    if (isServerSide(event.getEntity())) {
    Entity entity = event.getEntity();
    DimensionalRegion dimRegion = RegionDataManager.get().cacheFor(getEntityDim(entity)).getDimensionalRegion();
    FlagCheckEvent flagCheckEvent = checkTargetEvent(entity.blockPosition(), USE_PORTAL, dimRegion);
    event.setCanceled(flagCheckEvent.isDenied());
    if (event.isCanceled()) {
    if (entity instanceof PlayerEntity) {
    sendFlagDeniedMsg(flagCheckEvent, (PlayerEntity) entity);
    }
    return;
    }
    if (entity instanceof PlayerEntity) {
    FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent((PlayerEntity) entity, entity.blockPosition(), USE_PORTAL_PLAYERS, dimRegion);
    handleAndSendMsg(event, playerFlagCheckEvent);
    } else {

    if (entity instanceof ItemEntity) {
    flagCheckEvent = checkTargetEvent(entity.blockPosition(), USE_PORTAL_ITEMS, dimRegion);
    event.setCanceled(flagCheckEvent.isDenied());
    return;
    }
    if (isAnimal(entity)) {
    flagCheckEvent = checkTargetEvent(entity.blockPosition(), USE_PORTAL_ANIMALS, dimRegion);
    event.setCanceled(flagCheckEvent.isDenied());
    return;
    }
    if (isMonster(entity)) {
    flagCheckEvent = checkTargetEvent(entity.blockPosition(), USE_PORTAL_MONSTERS, dimRegion);
    event.setCanceled(flagCheckEvent.isDenied());
    return;
    }
    if (entity instanceof AbstractVillager) {
    flagCheckEvent = checkTargetEvent(entity.blockPosition(), USE_PORTAL_VILLAGERS, dimRegion);
    event.setCanceled(flagCheckEvent.isDenied());
    return;
    }
    if (entity instanceof AbstractMinecart) {
    flagCheckEvent = checkTargetEvent(entity.blockPosition(), USE_PORTAL_MINECARTS, dimRegion);
    event.setCanceled(flagCheckEvent.isDenied());
    }
    }
    }
    }


    public static void onTravelFromDim(EntityTravelToDimensionEvent event) {

    }

    public static void onTravelToDim(EntityTravelToDimensionEvent event) {

    }
     */


}
