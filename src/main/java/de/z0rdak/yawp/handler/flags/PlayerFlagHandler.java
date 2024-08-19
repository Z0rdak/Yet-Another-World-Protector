package de.z0rdak.yawp.handler.flags;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.util.MessageSender;
import net.minecraft.block.AbstractPressurePlateBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.IWaterLoggable;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.item.minecart.ContainerMinecartEntity;
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.merchant.villager.WanderingTraderEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.fluid.Fluid;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.state.properties.BlockStateProperties;
import net.minecraft.tags.FluidTags;
import net.minecraft.tags.ITag;
import net.minecraft.tags.ItemTags;
import net.minecraft.tileentity.EnderChestTileEntity;
import net.minecraft.tileentity.LecternTileEntity;
import net.minecraft.tileentity.LockableTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Hand;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.world.Explosion;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityMountEvent;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.*;
import net.minecraftforge.event.entity.player.*;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.event.world.ExplosionEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static net.minecraftforge.common.ToolType.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public final class PlayerFlagHandler {

    private PlayerFlagHandler() {
    }

    @SubscribeEvent
    public static void onElytraFlying(TickEvent.PlayerTickEvent event) {
        if (isServerSide(event.player) && event.phase == TickEvent.Phase.END) {
            RegistryKey<World> dim = getEntityDim(event.player);
            if (event.player.isFallFlying()) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.player.blockPosition(), NO_FLIGHT, dim, event.player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.player.stopFallFlying();
                });
            }
        }
    }

    /**
     * Prevents traditional attacks from players which use EntityPlayer.attackTargetEntityWithCurrentItem(Entity).
     */
    @SubscribeEvent
    public static void onAttackPlayer(AttackEntityEvent event) {
        if (notServerSideOrPlayerNull(event))
            return;
        if (event.getTarget() instanceof PlayerEntity) {
            PlayerEntity attacker = event.getPlayer();
            PlayerEntity target = (PlayerEntity) event.getTarget();
            RegistryKey<World> dim = getEntityDim(attacker);
            FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_PLAYERS, dim, attacker);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }

    }

    /**
     * Prevents various entities from been attacked from a player. <br>
     */
    @SubscribeEvent
    public static void onAttackEntity(AttackEntityEvent event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        Entity eventEntity = event.getTarget();
        RegistryKey<World> dim = getEntityDim(event.getPlayer());
        BlockPos entityPos = eventEntity.blockPosition();
        FlagCheckEvent checkEvent = null;

        if (isAnimal(eventEntity)) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_ANIMALS, dim, player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
        }
        if (isMonster(eventEntity)) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_MONSTERS, dim, player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
        }
        if (event.getTarget() instanceof VillagerEntity) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_VILLAGERS, dim, player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
        }
        if (event.getTarget() instanceof WanderingTraderEntity) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_WANDERING_TRADER, dim, player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
        }
        if (checkEvent != null) {
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }

    }

    @SubscribeEvent
    public static void onPickupItem(EntityItemPickupEvent event) {
        if (notServerSideOrPlayerNull(event))
            return;
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), ITEM_PICKUP, getEntityDim(event.getPlayer()), event.getPlayer());
        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
            event.setCanceled(true);
            MessageSender.sendFlagMsg(onDeny);
        });
    }

    @SubscribeEvent
    public static void onBreedingAttempt(BabyEntitySpawnEvent event) {
        PlayerEntity player = event.getCausedByPlayer();
        if (player == null) {
            return;
        }
        if (!player.getCommandSenderWorld().isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getParentB().blockPosition(), ANIMAL_BREEDING, getEntityDim(player), event.getCausedByPlayer());
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onAnimalTameAttempt(AnimalTameEvent event) {
        PlayerEntity player = event.getTamer();
        if (player == null) {
            return;
        }
        if (!player.getCommandSenderWorld().isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getAnimal().blockPosition(), ANIMAL_TAMING, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerLevelChange(PlayerXpEvent.LevelChange event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPlayer().blockPosition(), LEVEL_FREEZE, getEntityDim(player), player);
        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
            event.setCanceled(true);
            MessageSender.sendFlagMsg(onDeny);
        });
    }

    @SubscribeEvent
    public static void onPlayerXPChange(PlayerXpEvent.XpChange event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), XP_FREEZE, getEntityDim(player), player);
        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
            event.setCanceled(true);
            event.setAmount(0);
            MessageSender.sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onPlayerXpPickup(PlayerXpEvent.PickupXp event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), XP_PICKUP, getEntityDim(player), player);
        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
            event.setCanceled(true);
            event.getOrb().remove();
            MessageSender.sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onPvpAction(LivingHurtEvent event) {
        if (isServerSide(event)) {
            if (event.getSource() == null || event.getEntity() == null) return;
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            Entity hurtEntity = event.getEntityLiving();
            if (hurtEntity instanceof PlayerEntity && dmgSourceEntity instanceof PlayerEntity) {
                PlayerEntity playerTarget = (PlayerEntity) hurtEntity;
                PlayerEntity playerSource = (PlayerEntity) dmgSourceEntity;
                FlagCheckEvent checkEvent = new FlagCheckEvent(playerTarget.blockPosition(), NO_PVP, getEntityDim(playerSource), playerSource);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerHurt(LivingHurtEvent event) {
        if (isServerSide(event)) {
            Entity hurtEntity = event.getEntityLiving();
            if (hurtEntity instanceof PlayerEntity) {
                PlayerEntity playerTarget = (PlayerEntity) hurtEntity;
                FlagCheckEvent checkEvent = new FlagCheckEvent(playerTarget.blockPosition(), INVINCIBLE, getEntityDim(playerTarget), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
        }
    }


    /* TODO: Is this test even necessary anymore?
     *   - There is already a PVP flag for onHurt in place
     * */
    @SubscribeEvent
    public static void onReceiveDmg(LivingDamageEvent event) {
        if (isServerSide(event)) {
            if (event.getSource() == null || event.getEntity() == null) return;
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            if (dmgSourceEntity instanceof PlayerEntity && event.getEntityLiving() instanceof PlayerEntity) {
                PlayerEntity dmgTarget = (PlayerEntity) event.getEntityLiving();
                PlayerEntity dmgSource = ((PlayerEntity) dmgSourceEntity);
                FlagCheckEvent checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), MELEE_PLAYERS, getEntityDim(dmgSource), dmgSource);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                // another check for PVP - this does not prevent knock-back? but prevents dmg
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerKnockback(LivingKnockBackEvent event) {
        if (isServerSide(event)) {
            if (event.getEntityLiving() instanceof PlayerEntity) {
                PlayerEntity dmgTarget = (PlayerEntity) event.getEntityLiving();
                FlagCheckEvent checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), KNOCKBACK_PLAYERS, getEntityDim(dmgTarget), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setStrength(0);
                    MessageSender.sendFlagMsg(onDeny);
                });
                checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), INVINCIBLE, getEntityDim(dmgTarget), null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    event.setStrength(0);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerBreakBlock(BlockEvent.BreakEvent event) {
        if (isServerSide(event)) {
            if (event.getPlayer() == null) return;
            PlayerEntity player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), BREAK_BLOCKS, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                updateBlockState((World) event.getWorld(), event.getPos());
                MessageSender.sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerPlaceBlock(BlockEvent.EntityPlaceEvent event) {
        if (isServerSide(event)) {
            if (event.getEntity() == null || !(event.getEntity() instanceof PlayerEntity)) {
                return;
            }
            PlayerEntity player = (PlayerEntity) event.getEntity();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), PLACE_BLOCKS, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                updateBlockState((World) event.getWorld(), event.getPos());
                MessageSender.sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    public static void onEntityBreak(AttackEntityEvent event) {
        if (isServerSide(event)) {
            if (event.getTarget() == null || event.getEntity() == null) return;
            Entity target = event.getTarget();
            PlayerEntity player = event.getPlayer();            
            Set<String> entityTags = FlagConfig.getCoveredBlockEntityTags();
            boolean isCoveredByTag = entityTags.stream().anyMatch(entityTag -> {
                ResourceLocation tagRl = new ResourceLocation(entityTag);
                return target.getTags().contains(tagRl.getPath());
            });
            Set<String> entities = FlagConfig.getCoveredBlockEntities();
            boolean isBlockEntityCovered = entities.stream().anyMatch(entity -> {
                ResourceLocation entityRl = new ResourceLocation(entity);
                ResourceLocation targetRl = ForgeRegistries.ENTITIES.getKey(target.getType());
                return targetRl != null && targetRl.equals(entityRl);
            });
            if (isBlockEntityCovered || isCoveredByTag) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), BREAK_BLOCKS, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
        }
    }
    
    @SubscribeEvent
    public static void onExplosionStarted(ExplosionEvent.Start event) {
        if (isServerSide(event.getWorld())) {
            if (event.getExplosion() == null)
                return;
            Explosion explosion = event.getExplosion();
            BlockPos explosionPos = new BlockPos((int) explosion.getPosition().x, (int) explosion.getPosition().y, (int) explosion.getPosition().z);
            RegistryKey<World> dim = event.getWorld().dimension();
            if (explosion.getSourceMob() == null) {
                // source entity is null, but we still want to cancel the ignition
                FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, IGNITE_EXPLOSIVES, dim, null);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                });
            } else {
                if (explosion.getSourceMob() instanceof PlayerEntity) {
                    PlayerEntity player = (PlayerEntity) explosion.getSourceMob(); 
                    FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, IGNITE_EXPLOSIVES, dim, player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        MessageSender.sendFlagMsg(onDeny);
                    });
                }
                if (explosion.getSourceMob() instanceof MonsterEntity) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, MOB_GRIEFING, dim, null);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                    });
                } else {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, IGNITE_EXPLOSIVES, dim, null);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                    });
                }
            }
        }
    }

    @SubscribeEvent
    public static void onBonemealUse(BonemealEvent event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = (PlayerEntity) event.getEntity();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), USE_BONEMEAL, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        
    }

    @SubscribeEvent
    public static void onPlayerUseEnderPearl(EntityTeleportEvent event) {
        if (isServerSide(event)) {
            // handle player teleportation using ender pearls
            if (event instanceof EntityTeleportEvent.EnderPearl) {
                EntityTeleportEvent.EnderPearl enderPearlEvent = (EntityTeleportEvent.EnderPearl) event;
                if (enderPearlEvent.getPlayer() == null)
                    return;
                PlayerEntity player = enderPearlEvent.getPlayer();
                BlockPos target = new BlockPos((int) event.getTarget().x, (int) event.getTarget().y, (int) event.getTarget().z);
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, USE_ENDERPEARL_TO_REGION, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                FlagState flagState = processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
                if (flagState == FlagState.DENIED)
                    return;

                checkEvent = new FlagCheckEvent(new BlockPos(event.getTarget()), USE_ENDERPEARL_FROM_REGION, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
                /* FIXME: refund pearl - duplication bug with e.g. origins mod
                int count = player.getHeldItem(player.getActiveHand()).getCount();
                player.getHeldItem(player.getActiveHand()).setCount(count + 1);
                player.inventory.setChanged();
                return;
                */
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerUseToolSecondary(BlockEvent.BlockToolInteractEvent event) {
        if (isServerSide(event)) {
            PlayerEntity player = event.getPlayer();
            if (player == null) return;
            BlockPos target = event.getPos();
            RegistryKey<World> dim = getEntityDim(player);
            FlagCheckEvent checkEvent = new FlagCheckEvent(target, TOOL_SECONDARY_USE, dim, player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
            if (flagState == FlagState.DENIED)
                return;

            if (event.getToolType().equals(AXE)) {
                checkEvent = new FlagCheckEvent(target, AXE_STRIP, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (event.getToolType().equals(HOE)) {
                checkEvent = new FlagCheckEvent(target, HOE_TILL, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (event.getToolType().equals(SHOVEL)) {
                checkEvent = new FlagCheckEvent(target, SHOVEL_PATH, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        TileEntity targetEntity = event.getWorld().getBlockEntity(event.getPos());
        RegistryKey<World> dim = getEntityDim(player);
        boolean isLockableTileEntity = targetEntity instanceof LockableTileEntity;
        boolean isEnderChest = targetEntity instanceof EnderChestTileEntity;
        boolean isContainer = targetEntity instanceof LecternTileEntity || isLockableTileEntity;

        // used to allow player to place blocks when shift clicking container or usable bock
        boolean hasEmptyHands = hasEmptyHands(player);
        BlockRayTraceResult pos = event.getHitVec();
        boolean isBlock = pos != null && pos.getType() == RayTraceResult.Type.BLOCK;

        if (player.isShiftKeyDown() && hasEmptyHands || !player.isShiftKeyDown()) {
            if (isBlock) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(pos.getBlockPos(), USE_BLOCKS, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                    event.getWorld().updateNeighborsAt(pos.getBlockPos(), event.getWorld().getBlockState(pos.getBlockPos()).getBlock());
                });
            }
            // Note: following flags are already covered with use_blocks
            // check for ender chest access
            if (isEnderChest) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), ENDER_CHEST_ACCESS, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
            // check for container access
            if (isContainer) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), CONTAINER_ACCESS, dim, player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
        }
        if (!hasEmptyHands) {
            if (isBlock) {
                ItemStack itemInHand = player.getItemInHand(event.getHand());
                ResourceLocation itemRl = ForgeRegistries.ITEMS.getKey(itemInHand.getItem());
                Set<String> entities = FlagConfig.getCoveredBlockEntities();
                Set<String> entityTags = FlagConfig.getCoveredBlockEntityTags();
                boolean isCoveredByTag = entityTags.stream().anyMatch(tag -> {
                    ITag.INamedTag<Item> iTag = ItemTags.bind(tag);
                    return itemInHand.getItem().is(iTag);
                });                
                boolean isBlockCovered = entities.stream().anyMatch(entity -> {
                    ResourceLocation entityRl = new ResourceLocation(entity);              
                    return itemRl != null && itemRl.equals(entityRl);
                });                
                if (isBlockCovered || isCoveredByTag) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), PLACE_BLOCKS, dim, player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        MessageSender.sendFlagMsg(onDeny);
                        player.inventory.setChanged();
                    });
                }
            }
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, dim, player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
                player.inventory.setChanged();
            });
        }

    }

    @SubscribeEvent
    public static void onAccessMinecartChest(PlayerInteractEvent.EntityInteract event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        boolean isMinecartContainer = event.getTarget() instanceof ContainerMinecartEntity;
        if (isMinecartContainer) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), CONTAINER_ACCESS, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }

    }


    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteractSpecific event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), USE_ENTITIES, getEntityDim(player), player);
        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
            event.setCanceled(true);
            MessageSender.sendFlagMsg(onDeny);
        });
        if (!hasEmptyHands(player)) {

            checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }

    }

    private static boolean hasEmptyHands(PlayerEntity player) {
        return player.getItemInHand(Hand.MAIN_HAND).getItem().equals(Items.AIR)
                && player.getItemInHand(Hand.OFF_HAND).getItem().equals(Items.AIR);
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteract event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), USE_ENTITIES, getEntityDim(player), player);
        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
            event.setCanceled(true);
            MessageSender.sendFlagMsg(onDeny);
        });
        if (!hasEmptyHands(player)) {
            checkEvent = new FlagCheckEvent(event.getPos(), USE_ENTITIES, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }

    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.RightClickItem event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
            FlagCheckEvent checkEvent;
            if (!hasEmptyHands(player)) {
                checkEvent = new FlagCheckEvent(event.getPos(), USE_ENTITIES, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
            checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });


    }

    /**
     * Prevents players from using activator blocks like pressure plates
     * TODO: This is very jank implementation. Needs to be tested with multiple players. Move check to activator block itself
     */
    @SubscribeEvent
    public static void onSteppedOnActivator(BlockEvent.NeighborNotifyEvent event) {
        if (isServerSide(event)) {
            Block block = event.getWorld().getBlockState(event.getPos()).getBlock();
            BlockPos pos = event.getPos();
            if (block instanceof AbstractPressurePlateBlock) {
                AxisAlignedBB areaAbovePressurePlate = new AxisAlignedBB(pos.getX() - 1, pos.getY(), pos.getZ() - 1, pos.getX() + 1, pos.getY() + 2, pos.getZ() + 1);
                List<PlayerEntity> players = ((World) event.getWorld()).getEntities(EntityType.PLAYER, areaAbovePressurePlate, (player) -> true);
                final FlagState[] cumulativeState = {FlagState.UNDEFINED};
                Map<PlayerEntity, FlagCheckEvent> playerCheckEventMap = new HashMap<>();
                for (PlayerEntity player : players) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_BLOCKS, getEntityDim(player), player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    playerCheckEventMap.put(player, checkEvent);
                }
                for (Map.Entry<PlayerEntity, FlagCheckEvent> entry : playerCheckEventMap.entrySet()) {
                    FlagState state = HandlerUtil.processCheck(entry.getValue(), null, MessageSender::sendFlagMsg);
                    if (state == FlagState.DENIED) {
                        cumulativeState[0] = state;
                    }
                }
                if (cumulativeState[0] == FlagState.DENIED) {
                    event.setCanceled(true);
                }
            }
        }
    }

    /**
     * Note: Does not prevent from fluids generate additional blocks (cobble generator). Use BlockEvent.FluidPlaceBlockEvent for this
     * TODO: Check event.getFilledBucket again to maybe simplify this check
     */
    @SubscribeEvent
    public static void onBucketFill(FillBucketEvent event) {
        // Note: FilledBucket seems to always be null. use maxStackSize to determine bucket state (empty or filled)
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        if (event.getTarget() != null) {
            RayTraceResult pos = event.getTarget();
            BlockPos targetPos = new BlockPos(event.getTarget().getLocation());
            // MaxStackSize: 1 -> full bucket so only placeable; >1 -> empty bucket, only fillable
            int bucketItemMaxStackCount = event.getEmptyBucket().getMaxStackSize();
            // placing fluid
            if (bucketItemMaxStackCount == 1) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, PLACE_FLUIDS, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
            }
            // scooping fluid (breaking fluid)
            if (bucketItemMaxStackCount > 1) {
                boolean isWaterlogged = false;
                boolean isFluid = false;
                if (pos != null && pos.getType() == RayTraceResult.Type.BLOCK) {
                    BlockState blockState = event.getWorld().getBlockState(targetPos);
                    // check for waterlogged block
                    if (blockState.getBlock() instanceof IWaterLoggable) {
                        isWaterlogged = blockState.getValue(BlockStateProperties.WATERLOGGED);
                    }
                    // check if entityPos has a fluid tag
                    for (ITag.INamedTag<Fluid> tag : FluidTags.getWrappers()) {
                            if (blockState.getFluidState().getFluidState().is(tag)) {
                                isFluid = true;
                                break;
                            }
                    }
                    if (isWaterlogged || isFluid) {
                        FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, SCOOP_FLUIDS, getEntityDim(player), player);
                        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                            return;
                        }
                        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                            event.setCanceled(true);
                            MessageSender.sendFlagMsg(onDeny);
                        });
                    }
                }
            }
        }

    }

    /**
     * Note: message received from server but not distributed to all clients
     */
    @SubscribeEvent
    public static void onSendChat(ServerChatEvent event) {
        if (event.getPlayer() == null) return;
        ServerPlayerEntity player = event.getPlayer();
        FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), SEND_MESSAGE, getEntityDim(player), player);
        if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
            return;
        }
        HandlerUtil.processCheck(checkEvent, null, onDeny -> {
            event.setCanceled(true);
            MessageSender.sendFlagMsg(onDeny);
        });
    }

    @SubscribeEvent
    public static void onCommandSend(CommandEvent event) {
        try {
            PlayerEntity player = event.getParseResults().getContext().getSource().getPlayerOrException();
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), EXECUTE_COMMAND, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        } catch (CommandSyntaxException e) {
            // Most likely thrown because command was not send by a player.
            // This is fine because we don't want this flag to be triggered from non-players entities
        }
    }

    @SubscribeEvent
    public static void onPlayerAttemptSleep(SleepingTimeCheckEvent event) {
        if (notServerSideOrPlayerNull(event))
            return;
        PlayerEntity player = event.getPlayer();
        event.getSleepingLocation().ifPresent((pos) -> {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, SLEEP, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setResult(Event.Result.DENY);
                MessageSender.sendFlagMsg(onDeny);
            });
        });
    }

    @SubscribeEvent
    public static void onSetSpawn(PlayerSetSpawnEvent event) {
        if (notServerSideOrPlayerNull(event))
            return;
        BlockPos newSpawn = event.getNewSpawn();
        PlayerEntity player = event.getPlayer();
        if (newSpawn != null) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(newSpawn, SET_SPAWN, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                MessageSender.sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerDropItem(ItemTossEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            if (player == null) return;
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntityItem().blockPosition(), ITEM_DROP, getEntityDim(player), player);
            if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                return;
            }
            HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                event.setCanceled(true);
                player.addItem(event.getEntityItem().getItem());
                player.inventory.setChanged();
                MessageSender.sendFlagMsg(onDeny);
            });
        }
    }

    /**
     * Idea: Flags for different animals to mount
     */
    @SubscribeEvent
    public static void onEntityMountAttempt(EntityMountEvent event) {
        if (isServerSide(event)) {
            Entity entityBeingMounted = event.getEntityBeingMounted();
            if (event.getEntityMounting() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) event.getEntityMounting();
                FlagCheckEvent checkEvent = new FlagCheckEvent(entityBeingMounted.blockPosition(), ANIMAL_MOUNTING, getEntityDim(player), player);
                if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                    return;
                }
                HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                    event.setCanceled(true);
                    MessageSender.sendFlagMsg(onDeny);
                });
                if (event.isDismounting()) {
                    /* FIXME: Disabled in 1.16.5. Canceling event breaks unmounting. Wait for 1.17 fix: https://bugs.mojang.com/browse/MC-202202
                    checkEvent = new FlagCheckEvent(entityBeingMounted.blockPosition(), ANIMAL_UNMOUNTING, getEntityDim(player), player);
                    if (MinecraftForge.EVENT_BUS.post(checkEvent)) {
                        return;
                    }
                    HandlerUtil.processCheck(checkEvent, null, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                    */
                }
            }
        }
    }
}