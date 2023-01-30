package de.z0rdak.yawp.handler.flags;

public class GrievingFlagHandler {

    private GrievingFlagHandler() {
    }

    /*
    public static void onFarmLandTrampled(BlockEvent.FarmlandTrampleEvent event) {
        if (isServerSide(event.getEntity())) {
            Entity trampler = event.getEntity();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(trampler));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND, dimCache.getDimensionalRegion());
                event.setCanceled(flagCheckEvent.isDenied());
                if (event.isCanceled()) {
                    if (trampler instanceof PlayerEntity) {
                        sendFlagDeniedMsg(flagCheckEvent, (PlayerEntity) trampler);
                    }
                    return;
                }
                // cancel only player trampling
                if (trampler instanceof PlayerEntity player) {
                    FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent(player, event.getPos(), RegionFlag.TRAMPLE_FARMLAND_PLAYER, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, playerFlagCheckEvent);
                } else {
                    // cancel for other entities
                    flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.TRAMPLE_FARMLAND_OTHER, dimCache.getDimensionalRegion());
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }


    public static void onEntityDestroyBlock(LivingDestroyBlockEvent event) {
        if (isServerSide(event)) {
            LivingEntity destroyer = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(destroyer));
            if (dimCache != null) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                if (destroyer instanceof EnderDragonEntity) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.DRAGON_BLOCK_PROT, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (destroyer instanceof WitherEntity) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.WITHER_BLOCK_PROT, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    return;
                }
                if (destroyer instanceof ZombieEntity) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getPos(), RegionFlag.ZOMBIE_DOOR_PROT, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }

    /**
     * Idea: Flag for player not dropping loot as member/owner? -> local keepInventory

    public static void onEntityDropLoot(LivingDropsEvent event) {
        if (isServerSide(event)) {
            LivingEntity lootEntity = event.getEntityLiving();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(lootEntity));
            if (dimCache != null) {
                FlagCheckEvent flagCheckEvent = checkTargetEvent(lootEntity.getBlockPos(), RegionFlag.DROP_LOOT_ALL, dimCache.getDimensionalRegion());
                event.setCanceled(flagCheckEvent.isDenied());
                if (event.isCanceled()) {
                    return;
                }
                if (isPlayer(event.getSource().getEntity())) {
                    FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent((PlayerEntity) event.getSource().getEntity(), lootEntity.getBlockPos(), RegionFlag.DROP_LOOT_PLAYER, dimCache.getDimensionalRegion());
                    handleAndSendMsg(event, playerFlagCheckEvent);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEntityXpDrop(LivingExperienceDropEvent event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getAttackingPlayer();
            Entity xpDroppingEntity = event.getEntityLiving();
            if (player != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                if (dimCache != null) {
                    DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
                    // prevent all xp drop
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_ALL, dimRegion);
                    event.setCanceled(flagCheckEvent.isDenied());
                    if (event.isCanceled()) {
                        return;
                    }
                    if (event.getAttackingPlayer() != null) {
                        // prevent non-member/owner players from dropping xp by killing mobs
                        FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent(player, xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_PLAYER, dimCache.getDimensionalRegion());
                        if (handleAndSendMsg(event, playerFlagCheckEvent)) {
                            return;
                        }
                    }
                    // prevent monster xp drop
                    if (isMonster(xpDroppingEntity)) {
                        flagCheckEvent = checkTargetEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_MONSTER, dimRegion);
                        event.setCanceled(flagCheckEvent.isDenied());
                    } else {
                        // prevent other entity xp drop (villagers, animals, ..)
                        flagCheckEvent = checkTargetEvent(xpDroppingEntity.blockPosition(), RegionFlag.XP_DROP_OTHER, dimRegion);
                        event.setCanceled(flagCheckEvent.isDenied());
                    }
                }
            }
        }
    }

    @SubscribeEvent
    public static void onEndermanPlacingBlock(BlockEvent.EntityPlaceEvent event) {
        if (isServerSide(event)) {
            if (event.getEntity() instanceof EnderMan) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(event.getEntity()));
                if (dimCache != null) {
                    FlagCheckEvent flagCheckEvent = checkTargetEvent(event.getEntity().blockPosition(), RegionFlag.ENTITY_PLACE, dimCache.getDimensionalRegion());
                    event.setCanceled(flagCheckEvent.isDenied());
                }
            }
        }
    }

    /**
     * TODO: Inverted flags would need to re-add allowed blocks/entites
     * Removes affected entities and/or blocks from the event list to protect them

    @SubscribeEvent
    public static void onExplosion(ExplosionEvent.Detonate event) {
        if (!event.getWorld().isClientSide) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(event.getWorld().dimension());
            if (dimCache != null && dimCache.getDimensionalRegion().isActive()) {
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

                Set<BlockPos> protectedBlocks = event.getAffectedBlocks().stream()
                        .filter(blockPos -> checkTargetEventFor(blockPos, RegionFlag.EXPLOSION_BLOCK, dimRegion))
                        .collect(Collectors.toSet());
                Set<Entity> protectedEntities = event.getAffectedEntities().stream()
                        .filter(entity -> checkTargetEventFor(entity.blockPosition(), RegionFlag.EXPLOSION_ENTITY, dimRegion))
                        .collect(Collectors.toSet());

                event.getAffectedBlocks().removeAll(protectedBlocks);
                event.getAffectedEntities().removeAll(protectedEntities);

                if (event.getExplosion().getSourceMob() != null) {
                    boolean explosionTriggeredByCreeper = (event.getExplosion().getSourceMob() instanceof Creeper);
                    if (explosionTriggeredByCreeper) {
                        protectedBlocks = event.getAffectedBlocks().stream()
                                .filter(blockPos -> checkTargetEventFor(blockPos, RegionFlag.EXPLOSION_CREEPER_BLOCK, dimRegion))
                                .collect(Collectors.toSet());
                        protectedEntities = event.getAffectedEntities().stream()
                                .filter(entity -> checkTargetEventFor(entity.blockPosition(), RegionFlag.EXPLOSION_CREEPER_ENTITY, dimRegion))
                                .collect(Collectors.toSet());
                    } else {
                        protectedBlocks = event.getAffectedBlocks().stream()
                                .filter(blockPos -> checkTargetEventFor(blockPos, RegionFlag.EXPLOSION_OTHER_BLOCKS, dimRegion))
                                .collect(Collectors.toSet());
                        protectedEntities = event.getAffectedEntities().stream()
                                .filter(entity -> checkTargetEventFor(entity.blockPosition(), RegionFlag.EXPLOSION_OTHER_ENTITY, dimRegion))
                                .collect(Collectors.toSet());
                    }
                    event.getAffectedBlocks().removeAll(protectedBlocks);
                    event.getAffectedEntities().removeAll(protectedEntities);
                }
            }
        }
    }

     */
}
